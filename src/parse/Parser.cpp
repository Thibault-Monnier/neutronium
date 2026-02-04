#include "Parser.hpp"

#include <algorithm>
#include <cassert>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <initializer_list>
#include <memory>
#include <optional>
#include <system_error>
#include <utility>
#include <vector>

#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "lex/Token.hpp"
#include "lex/TokenKind.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"

#if defined(__GNUC__) || defined(__clang__)
#define EXPECT_OR_RETURN_NULLPTR(tokenKind)            \
    __extension__({                                    \
        Token _tok_ = Token::dummy();                  \
        if (!expect(tokenKind, _tok_)) return nullptr; \
        _tok_;                                         \
    })
#else
#error "EXPECT_OR_RETURN_NULLPTR is not supported by your compiler."
#endif

AST::CompilationUnit* Parser::parse() {
    const auto ast = parseCompilationUnit();

    if (diagnosticsEngine_.hasErrors()) {
        diagnosticsEngine_.emit();
        exit(EXIT_FAILURE);
    }

    assert(ast && "AST should not be null here");

    return ast;
}

__attribute__((always_inline)) bool Parser::advanceIf(const TokenKind expected) {
    const Token token = peek();

    const bool matches = token.kind() == expected;
    if (matches) advance();

    return matches;
}

__attribute__((always_inline)) bool Parser::expect(const TokenKind expected, Token& outToken) {
    const Token token = peek();
    outToken = token;

    if (token.kind() != expected) [[unlikely]] {
        expectError(expected);
        return false;
    }

    advance();
    return true;
}

template <typename T>
std::optional<std::vector<T*>> Parser::parseCommaSeparatedList(
    const TokenKind endDelimiter, const std::function<T*()>& parseElement) {
    std::vector<T*> elements;
    while (peek().kind() != endDelimiter) {
        auto elem = parseElement();
        if (!elem) return std::nullopt;
        elements.push_back(elem);
        if (!advanceIf(TokenKind::COMMA)) break;
    }
    return elements;
}

std::optional<std::vector<AST::Expression*>> Parser::parseExpressionList(
    const TokenKind endDelimiter) {
    return parseCommaSeparatedList<AST::Expression>(endDelimiter,
                                                    [this] { return parseExpression(); });
}

std::optional<Type> Parser::tryParsePrimitiveType(const TokenKind tokenKind) {
    switch (tokenKind) {
        case TokenKind::INT:
            return Type::intType();
        case TokenKind::INT8:
            return Type::int8Type();
        case TokenKind::INT16:
            return Type::int16Type();
        case TokenKind::INT32:
            return Type::int32Type();
        case TokenKind::INT64:
            return Type::int64Type();
        case TokenKind::BOOL:
            return Type::boolType();
        default:
            return std::nullopt;
    }
}

std::unique_ptr<Type> Parser::parseTypeSpecifier() {
    const TokenKind tokenKind = peek().kind();

    if (auto type = tryParsePrimitiveType(tokenKind)) {
        advance();
        return std::make_unique<Type>(*type);
    }

    if (advanceIf(TokenKind::LEFT_BRACKET)) {
        const auto elementType = parseTypeSpecifier();
        if (!elementType) return nullptr;

        const TypeID elementTypeID = typeManager_.createType(*elementType);
        EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

        const auto arrayLength = parseNumberLiteral();
        if (!arrayLength) return nullptr;

        EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACKET);
        return std::make_unique<Type>(elementTypeID, arrayLength->value_);
    }

    return invalidTypeSpecifierError();
}

std::optional<Type> Parser::maybeParseTypeAnnotation(const TokenKind typeAnnotationIndicator,
                                                     Type defaultType) {
    if (advanceIf(typeAnnotationIndicator)) {
        const auto parsedType = parseTypeSpecifier();
        if (!parsedType) return std::nullopt;
        return *parsedType;
    }
    return defaultType;
}

AST::Identifier* Parser::parseIdentifier() {
    const Token ident = EXPECT_OR_RETURN_NULLPTR(TokenKind::IDENTIFIER);
    return astArena_.insert<AST::Identifier>(ident.lexeme(sourceCode_), ident.byteOffsetStart(),
                                             ident.byteOffsetEnd(), fileID_, generateAnyType());
}

AST::NumberLiteral* Parser::parseNumberLiteral() {
    const Token token = EXPECT_OR_RETURN_NULLPTR(TokenKind::NUMBER_LITERAL);
    const std::string_view lexeme = token.lexeme(sourceCode_);

    int64_t value;
    const std::from_chars_result res =
        std::from_chars(lexeme.data(), lexeme.data() + lexeme.size(), value);

    if (res.ec != std::errc{}) [[unlikely]] {
        return invalidNumberLiteralError(token);
    }

    return astArena_.insert<AST::NumberLiteral>(value, token.byteOffsetStart(),
                                                token.byteOffsetEnd(), fileID_, generateAnyType());
}

AST::ArrayLiteral* Parser::parseArrayLiteral() {
    const Token lBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACKET);
    auto elements = parseExpressionList(TokenKind::RIGHT_BRACKET);
    if (!elements) return nullptr;
    const Token rBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACKET);

    return astArena_.insert<AST::ArrayLiteral>(insertVector(std::move(elements.value())),
                                               lBracket.byteOffsetStart(), rBracket.byteOffsetEnd(),
                                               fileID_, generateAnyType());
}

AST::Expression* Parser::parseIdentifierOrFunctionCall() {
    auto ident = parseIdentifier();
    if (!ident) return nullptr;

    if (advanceIf(TokenKind::LEFT_PAREN)) {
        // Function call
        auto arguments = parseExpressionList(TokenKind::RIGHT_PAREN);
        if (!arguments) return nullptr;
        const Token rParen = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_PAREN);

        const uint32_t startIndex = ident->sourceStartIndex();
        return astArena_.insert<AST::FunctionCall>(
            ident, insertVector(std::move(arguments.value())), startIndex, rParen.byteOffsetEnd(),
            fileID_, generateAnyType());
    } else {
        // Identifier
        return ident;
    }
}

AST::ArrayAccess* Parser::parseArrayAccess(const AST::Expression* base) {
    EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACKET);
    auto index = parseExpression();
    if (!index) return nullptr;
    const Token rBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACKET);

    const uint32_t startIndex = base->sourceStartIndex();
    return astArena_.insert<AST::ArrayAccess>(base, index, startIndex, rBracket.byteOffsetEnd(),
                                              fileID_, generateAnyType());
}

AST::Expression* Parser::parsePrimaryExpression() {
    const Token token = peek();

    switch (token.kind()) {
        case TokenKind::NUMBER_LITERAL:
            return parseNumberLiteral();

        case TokenKind::LEFT_BRACKET:
            return parseArrayLiteral();

        case TokenKind::TRUE:
        case TokenKind::FALSE: {
            const bool value = token.kind() == TokenKind::TRUE;
            advance();
            return astArena_.insert<AST::BooleanLiteral>(
                value, token.byteOffsetStart(), token.byteOffsetEnd(), fileID_, generateAnyType());
        }

        case TokenKind::IDENTIFIER:
            return parseIdentifierOrFunctionCall();

        case TokenKind::LEFT_PAREN: {
            EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_PAREN);
            auto inner = parseExpression();
            if (!inner) return nullptr;
            EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_PAREN);
            return inner;
        }

        default:
            return invalidPrimaryExpressionError();
    }
}

AST::Expression* Parser::parsePostfixExpression() {
    auto postfixExpr = parsePrimaryExpression();
    if (!postfixExpr) return nullptr;

    while (true) {
        const Token token = peek();
        if (token.kind() == TokenKind::LEFT_BRACKET) {
            postfixExpr = parseArrayAccess(postfixExpr);
            if (!postfixExpr) return nullptr;
        } else {
            break;
        }
    }

    return postfixExpr;
}

AST::Expression* Parser::parseUnaryExpression() {
    const Token token = peek();

    const AST::Operator op = AST::tokenKindToOperator(token.kind());
    if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
        op == AST::Operator::LOGICAL_NOT) {
        advance();
        auto operand = parsePostfixExpression();
        if (!operand) return nullptr;

        const uint32_t endIndex = operand->sourceEndIndex();
        return astArena_.insert<AST::UnaryExpression>(op, operand, token.byteOffsetStart(),
                                                      endIndex, fileID_, generateAnyType());
    }

    return parsePostfixExpression();
}

AST::Expression* Parser::parseBinaryExpression(
    const std::function<AST::Expression*()>& parseOperand,
    const std::initializer_list<AST::Operator> allowedOps, const bool allowMultiple) {
    auto left = parseOperand();
    if (!left) return nullptr;

    while (true) {
        const AST::Operator op = AST::tokenKindToOperator(peek().kind());
        if (std::ranges::find(allowedOps, op) == allowedOps.end()) break;

        advance();
        auto right = parseOperand();
        if (!right) return nullptr;

        const uint32_t startIndex = left->sourceStartIndex();
        const uint32_t endIndex = right->sourceEndIndex();
        left = astArena_.insert<AST::BinaryExpression>(left, op, right, startIndex, endIndex,
                                                       fileID_, generateAnyType());
        if (!allowMultiple) break;
    }

    return left;
}

AST::Expression* Parser::parseMultiplicativeExpression() {
    return parseBinaryExpression([this] { return parseUnaryExpression(); },
                                 {AST::Operator::MULTIPLY, AST::Operator::DIVIDE}, true);
}

AST::Expression* Parser::parseAdditiveExpression() {
    return parseBinaryExpression([this] { return parseMultiplicativeExpression(); },
                                 {AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
}

AST::Expression* Parser::parseComparisonExpression() {
    return parseBinaryExpression(
        [this] { return parseAdditiveExpression(); },
        {AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
         AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
         AST::Operator::GREATER_THAN_OR_EQUAL},
        false);
}

AST::Expression* Parser::parseLogicalExpression() {
    auto left = parseComparisonExpression();
    if (!left) return nullptr;

    const AST::Operator exprOp = AST::tokenKindToOperator(peek().kind());
    if (exprOp != AST::Operator::LOGICAL_OR && exprOp != AST::Operator::LOGICAL_AND) {
        return left;
    }

    while (true) {
        if (AST::tokenKindToOperator(peek().kind()) != exprOp) break;

        advance();
        auto right = parseComparisonExpression();
        if (!right) return nullptr;

        const uint32_t startIndex = left->sourceStartIndex();
        const uint32_t endIndex = right->sourceEndIndex();
        left = astArena_.insert<AST::BinaryExpression>(left, exprOp, right, startIndex, endIndex,
                                                       fileID_, generateAnyType());
    }

    return left;
}

AST::Expression* Parser::parseExpression() { return parseLogicalExpression(); }

AST::VariableDefinition* Parser::parseVariableDefinition() {
    const Token let = EXPECT_OR_RETURN_NULLPTR(TokenKind::LET);

    const bool isMutable = advanceIf(TokenKind::MUT);

    auto identifier = parseIdentifier();
    if (!identifier) return nullptr;

    const auto parsedType = maybeParseTypeAnnotation(TokenKind::COLON, Type::anyFamilyType());
    if (!parsedType) return nullptr;

    const TypeID typeID = typeManager_.createType(*parsedType);

    EXPECT_OR_RETURN_NULLPTR(TokenKind::EQUAL);
    auto value = parseExpression();
    if (!value) return nullptr;

    const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

    return astArena_.insert<AST::VariableDefinition>(
        identifier, typeID, isMutable, value, let.byteOffsetStart(), semi.byteOffsetEnd(), fileID_);
}

AST::Statement* Parser::parseAssignmentOrExpressionStatement() {
    auto expression = parseExpression();
    if (!expression) return nullptr;

    const Token nextToken = peek();
    const AST::Operator op = AST::tokenKindToOperator(nextToken.kind());
    if (AST::isAssignmentOperator(op)) {
        // Assignment
        advance();

        auto right = parseExpression();
        if (!right) return nullptr;
        const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

        const uint32_t startIndex = expression->sourceStartIndex();
        return astArena_.insert<AST::Assignment>(expression, op, right, startIndex,
                                                 semi.byteOffsetEnd(), fileID_);
    } else {
        // Expression statement
        const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
        const uint32_t startIndex = expression->sourceStartIndex();
        return astArena_.insert<AST::ExpressionStatement>(expression, startIndex,
                                                          semi.byteOffsetEnd(), fileID_);
    }
}

AST::BlockStatement* Parser::parseElseClause() {
    if (advanceIf(TokenKind::ELSE)) {
        EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);
        return parseBlockStatement();
    }

    if (peek().kind() == TokenKind::ELIF) {
        auto elif = parseIfOrElif(TokenKind::ELIF);
        if (!elif) return nullptr;

        const uint32_t start = elif->sourceStartIndex();
        const uint32_t end = elif->sourceEndIndex();

        std::vector<AST::Statement*> stmts;
        stmts.push_back(elif);

        return astArena_.insert<AST::BlockStatement>(insertVector(std::move(stmts)), start, end,
                                                     fileID_);
    }

    std::unreachable();
}

AST::IfStatement* Parser::parseIfOrElif(const TokenKind kind) {
    const Token keywordTok = EXPECT_OR_RETURN_NULLPTR(kind);

    auto condition = parseExpression();
    if (!condition) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    auto body = parseBlockStatement();
    if (!body) return nullptr;

    const AST::BlockStatement* elseClause = nullptr;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        elseClause = parseElseClause();
        if (!elseClause) return nullptr;
    }

    const uint32_t endIndex = elseClause ? elseClause->sourceEndIndex() : body->sourceEndIndex();
    return astArena_.insert<AST::IfStatement>(condition, body, elseClause,
                                              keywordTok.byteOffsetStart(), endIndex, fileID_);
}

AST::IfStatement* Parser::parseIfStatement() { return parseIfOrElif(TokenKind::IF); }

AST::WhileStatement* Parser::parseWhileStatement() {
    const Token whileTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::WHILE);

    auto condition = parseExpression();
    if (!condition) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    auto body = parseBlockStatement();
    if (!body) return nullptr;

    const uint32_t endIndex = body->sourceEndIndex();
    return astArena_.insert<AST::WhileStatement>(condition, body, whileTok.byteOffsetStart(),
                                                 endIndex, fileID_);
}

AST::BreakStatement* Parser::parseBreakStatement() {
    const Token breakTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::BREAK);
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return astArena_.insert<AST::BreakStatement>(breakTok.byteOffsetStart(),
                                                 semiTok.byteOffsetEnd(), fileID_);
}

AST::ContinueStatement* Parser::parseContinueStatement() {
    const Token continueTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::CONTINUE);
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return astArena_.insert<AST::ContinueStatement>(continueTok.byteOffsetStart(),
                                                    semiTok.byteOffsetEnd(), fileID_);
}

AST::ReturnStatement* Parser::parseReturnStatement() {
    const Token returnTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::RETURN);

    const AST::Expression* returnValue = nullptr;
    if (peek().kind() != TokenKind::SEMICOLON) {
        returnValue = parseExpression();
        if (!returnValue) return nullptr;
    }

    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

    return astArena_.insert<AST::ReturnStatement>(returnValue, returnTok.byteOffsetStart(),
                                                  semiTok.byteOffsetEnd(), fileID_);
}

AST::ExitStatement* Parser::parseExitStatement() {
    const Token exitTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::EXIT);
    auto exitCode = parseExpression();
    if (!exitCode) return nullptr;
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return astArena_.insert<AST::ExitStatement>(exitCode, exitTok.byteOffsetStart(),
                                                semiTok.byteOffsetEnd(), fileID_);
}

AST::BlockStatement* Parser::parseBlockStatement() {
    const Token lBrace = EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACE);

    std::vector<AST::Statement*> statements;
    while (peek().kind() != TokenKind::EOF_ && peek().kind() != TokenKind::RIGHT_BRACE) {
        if (auto stmt = parseStatement()) {
            statements.push_back(stmt);
        } else {
            // Error recovery: skip until top level ';' or '}' that closes this block
            int depth = 0;
            while (peek().kind() != TokenKind::EOF_) {
                const TokenKind tokenKind = peek().kind();
                if (tokenKind == TokenKind::LEFT_BRACE) {
                    depth++;
                } else if (tokenKind == TokenKind::RIGHT_BRACE) {
                    if (depth == 0) break;
                    depth--;
                } else if (tokenKind == TokenKind::SEMICOLON && depth == 0) {
                    advance();  // Consume the semicolon
                    break;
                }
                advance();
            }
        }
    }

    const Token rBrace = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACE);

    return astArena_.insert<AST::BlockStatement>(insertVector(std::move(statements)),
                                                 lBrace.byteOffsetStart(), rBrace.byteOffsetEnd(),
                                                 fileID_);
}

AST::Statement* Parser::parseStatement() {
    const TokenKind tokenKind = peek().kind();

    if (tokenKind == TokenKind::LET) return parseVariableDefinition();
    if (tokenKind == TokenKind::IF) return parseIfStatement();
    if (tokenKind == TokenKind::WHILE) return parseWhileStatement();
    if (tokenKind == TokenKind::BREAK) return parseBreakStatement();
    if (tokenKind == TokenKind::CONTINUE) return parseContinueStatement();
    if (tokenKind == TokenKind::RETURN) return parseReturnStatement();
    if (tokenKind == TokenKind::EXIT) return parseExitStatement();
    if (tokenKind == TokenKind::LEFT_BRACE) return parseBlockStatement();
    return parseAssignmentOrExpressionStatement();
}

AST::VariableDefinition* Parser::parseFunctionParameter() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isMutable = advanceIf(TokenKind::MUT);

    auto identifier = parseIdentifier();
    if (!identifier) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    const auto parsedType = parseTypeSpecifier();
    if (!parsedType) return nullptr;
    const TypeID typeID = typeManager_.createType(*parsedType);

    const uint32_t endIndex = identifier->sourceEndIndex();
    return astArena_.insert<AST::VariableDefinition>(identifier, typeID, isMutable,
                                                     sourceStartIndex, endIndex, fileID_);
}

std::unique_ptr<ParsedFunctionSignature> Parser::parseFunctionSignature() {
    auto identifier = parseIdentifier();
    if (!identifier) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_PAREN);
    auto parameters = parseCommaSeparatedList<AST::VariableDefinition>(
        TokenKind::RIGHT_PAREN, [this] { return parseFunctionParameter(); });
    if (!parameters) return nullptr;
    EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_PAREN);

    const auto returnType = maybeParseTypeAnnotation(TokenKind::RIGHT_ARROW, Type::voidType());
    if (!returnType) return nullptr;

    return std::make_unique<ParsedFunctionSignature>(identifier, std::move(parameters.value()),
                                                     typeManager_.createType(returnType.value()));
}

AST::ExternalFunctionDeclaration* Parser::parseExternalFunctionDeclaration() {
    const Token externTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::EXTERN);
    EXPECT_OR_RETURN_NULLPTR(TokenKind::FN);

    const auto signature = parseFunctionSignature();
    if (!signature) return nullptr;

    const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

    return astArena_.insert<AST::ExternalFunctionDeclaration>(
        signature->identifier_, insertVector(std::move(signature->parameters_)),
        signature->returnTypeID_, externTok.byteOffsetStart(), semi.byteOffsetEnd(), fileID_);
}

AST::FunctionDefinition* Parser::parseFunctionDefinition() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isExported = advanceIf(TokenKind::EXPORT);

    EXPECT_OR_RETURN_NULLPTR(TokenKind::FN);

    const auto signature = parseFunctionSignature();
    if (!signature) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);
    auto body = parseBlockStatement();
    if (!body) return nullptr;

    const uint32_t endIndex = body->sourceEndIndex();
    return astArena_.insert<AST::FunctionDefinition>(
        signature->identifier_, insertVector(std::move(signature->parameters_)),
        signature->returnTypeID_, isExported, body, sourceStartIndex, endIndex, fileID_);
}

AST::CompilationUnit* Parser::parseCompilationUnit() {
    std::vector<AST::ExternalFunctionDeclaration*> externalFunctions;
    std::vector<AST::FunctionDefinition*> functions;

    while (peek().kind() == TokenKind::EXTERN) {
        if (const auto externFunction = parseExternalFunctionDeclaration()) {
            externalFunctions.push_back(externFunction);
        } else {
            // Error recovery: skip to the next semicolon
            while (peek().kind() != TokenKind::SEMICOLON && peek().kind() != TokenKind::EOF_) {
                advance();
            }
            advanceIf(TokenKind::SEMICOLON);
        }
    }

    while (peek().kind() != TokenKind::EOF_) {
        if (const auto functionDefinition = parseFunctionDefinition()) {
            functions.push_back(functionDefinition);

        } else {
            // Error recovery: skip to the next function definition
            while (peek().kind() != TokenKind::FN && peek().kind() != TokenKind::EXPORT &&
                   peek().kind() != TokenKind::EOF_) {
                advance();
            }
        }
    }

    return astArena_.insert<AST::CompilationUnit>(insertVector(std::move(externalFunctions)),
                                                  insertVector(std::move(functions)), fileID_,
                                                  sourceCode_.size());
}
