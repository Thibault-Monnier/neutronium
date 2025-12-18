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

std::unique_ptr<AST::Program> Parser::parse() {
    auto ast = parseProgram();

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
std::optional<std::vector<std::unique_ptr<T>>> Parser::parseCommaSeparatedList(
    const TokenKind endDelimiter, const std::function<std::unique_ptr<T>()>& parseElement) {
    std::vector<std::unique_ptr<T>> elements;
    while (peek().kind() != endDelimiter) {
        auto elem = parseElement();
        if (!elem) return std::nullopt;
        elements.push_back(std::move(elem));
        if (!advanceIf(TokenKind::COMMA)) break;
    }
    return elements;
}

std::optional<std::vector<std::unique_ptr<AST::Expression>>> Parser::parseExpressionList(
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

std::unique_ptr<AST::Identifier> Parser::parseIdentifier() {
    const Token ident = EXPECT_OR_RETURN_NULLPTR(TokenKind::IDENTIFIER);
    return std::make_unique<AST::Identifier>(ident.lexeme(sourceCode_), ident.byteOffsetStart(),
                                             ident.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::NumberLiteral> Parser::parseNumberLiteral() {
    const Token token = EXPECT_OR_RETURN_NULLPTR(TokenKind::NUMBER_LITERAL);
    const std::string_view lexeme = token.lexeme(sourceCode_);

    int64_t value;
    const std::from_chars_result res =
        std::from_chars(lexeme.data(), lexeme.data() + lexeme.size(), value);

    if (res.ec != std::errc{}) [[unlikely]] {
        return invalidNumberLiteralError(token);
    }

    return std::make_unique<AST::NumberLiteral>(value, token.byteOffsetStart(),
                                                token.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::ArrayLiteral> Parser::parseArrayLiteral() {
    const Token lBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACKET);
    auto elements = parseExpressionList(TokenKind::RIGHT_BRACKET);
    if (!elements) return nullptr;
    const Token rBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayLiteral>(std::move(elements.value()),
                                               lBracket.byteOffsetStart(), rBracket.byteOffsetEnd(),
                                               generateAnyType());
}

std::unique_ptr<AST::Expression> Parser::parseIdentifierOrFunctionCall() {
    auto ident = parseIdentifier();
    if (!ident) return nullptr;

    if (advanceIf(TokenKind::LEFT_PAREN)) {
        // Function call
        auto arguments = parseExpressionList(TokenKind::RIGHT_PAREN);
        if (!arguments) return nullptr;
        const Token rParen = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_PAREN);

        const uint32_t startIndex = ident->sourceStartIndex();
        return std::make_unique<AST::FunctionCall>(std::move(ident), std::move(arguments.value()),
                                                   startIndex, rParen.byteOffsetEnd(),
                                                   generateAnyType());
    } else {
        // Identifier
        return ident;
    }
}

std::unique_ptr<AST::ArrayAccess> Parser::parseArrayAccess(std::unique_ptr<AST::Expression> base) {
    EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACKET);
    auto index = parseExpression();
    if (!index) return nullptr;
    const Token rBracket = EXPECT_OR_RETURN_NULLPTR(TokenKind::RIGHT_BRACKET);

    const uint32_t startIndex = base->sourceStartIndex();
    return std::make_unique<AST::ArrayAccess>(std::move(base), std::move(index), startIndex,
                                              rBracket.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::Expression> Parser::parsePrimaryExpression() {
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
            return std::make_unique<AST::BooleanLiteral>(value, token.byteOffsetStart(),
                                                         token.byteOffsetEnd(), generateAnyType());
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

std::unique_ptr<AST::Expression> Parser::parsePostfixExpression() {
    auto postfixExpr = parsePrimaryExpression();
    if (!postfixExpr) return nullptr;

    while (true) {
        const Token token = peek();
        if (token.kind() == TokenKind::LEFT_BRACKET) {
            postfixExpr = parseArrayAccess(std::move(postfixExpr));
            if (!postfixExpr) return nullptr;
        } else {
            break;
        }
    }

    return postfixExpr;
}

std::unique_ptr<AST::Expression> Parser::parseUnaryExpression() {
    const Token token = peek();

    const AST::Operator op = AST::tokenKindToOperator(token.kind());
    if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
        op == AST::Operator::LOGICAL_NOT) {
        advance();
        auto operand = parsePostfixExpression();
        if (!operand) return nullptr;

        const uint32_t endIndex = operand->sourceEndIndex();
        return std::make_unique<AST::UnaryExpression>(
            op, std::move(operand), token.byteOffsetStart(), endIndex, generateAnyType());
    }

    return parsePostfixExpression();
}

std::unique_ptr<AST::Expression> Parser::parseBinaryExpression(
    const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
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
        left = std::make_unique<AST::BinaryExpression>(std::move(left), op, std::move(right),
                                                       startIndex, endIndex, generateAnyType());
        if (!allowMultiple) break;
    }

    return left;
}

std::unique_ptr<AST::Expression> Parser::parseMultiplicativeExpression() {
    return parseBinaryExpression([this] { return parseUnaryExpression(); },
                                 {AST::Operator::MULTIPLY, AST::Operator::DIVIDE}, true);
}

std::unique_ptr<AST::Expression> Parser::parseAdditiveExpression() {
    return parseBinaryExpression([this] { return parseMultiplicativeExpression(); },
                                 {AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
}

std::unique_ptr<AST::Expression> Parser::parseComparisonExpression() {
    return parseBinaryExpression(
        [this] { return parseAdditiveExpression(); },
        {AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
         AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
         AST::Operator::GREATER_THAN_OR_EQUAL},
        false);
}

std::unique_ptr<AST::Expression> Parser::parseExpression() { return parseComparisonExpression(); }

std::unique_ptr<AST::VariableDefinition> Parser::parseVariableDefinition() {
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

    return std::make_unique<AST::VariableDefinition>(std::move(identifier), typeID, isMutable,
                                                     std::move(value), let.byteOffsetStart(),
                                                     semi.byteOffsetEnd());
}

std::unique_ptr<AST::Statement> Parser::parseAssignmentOrExpressionStatement() {
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
        return std::make_unique<AST::Assignment>(std::move(expression), op, std::move(right),
                                                 startIndex, semi.byteOffsetEnd());
    } else {
        // Expression statement
        const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
        const uint32_t startIndex = expression->sourceStartIndex();
        return std::make_unique<AST::ExpressionStatement>(std::move(expression), startIndex,
                                                          semi.byteOffsetEnd());
    }
}

std::unique_ptr<AST::BlockStatement> Parser::parseElseClause() {
    if (advanceIf(TokenKind::ELSE)) {
        EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);
        return parseBlockStatement();
    }

    if (peek().kind() == TokenKind::ELIF) {
        auto elif = parseIfOrElif(TokenKind::ELIF);
        if (!elif) return nullptr;

        const uint32_t start = elif->sourceStartIndex();
        const uint32_t end = elif->sourceEndIndex();

        std::vector<std::unique_ptr<AST::Statement>> stmts;
        stmts.push_back(std::move(elif));

        return std::make_unique<AST::BlockStatement>(std::move(stmts), start, end);
    }

    std::unreachable();
}

std::unique_ptr<AST::IfStatement> Parser::parseIfOrElif(const TokenKind kind) {
    const Token keywordTok = EXPECT_OR_RETURN_NULLPTR(kind);

    auto condition = parseExpression();
    if (!condition) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    auto body = parseBlockStatement();
    if (!body) return nullptr;

    std::unique_ptr<AST::BlockStatement> elseClause;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        elseClause = parseElseClause();
        if (!elseClause) return nullptr;
    }

    const uint32_t endIndex = elseClause ? elseClause->sourceEndIndex() : body->sourceEndIndex();
    return std::make_unique<AST::IfStatement>(std::move(condition), std::move(body),
                                              std::move(elseClause), keywordTok.byteOffsetStart(),
                                              endIndex);
}

std::unique_ptr<AST::IfStatement> Parser::parseIfStatement() {
    return parseIfOrElif(TokenKind::IF);
}

std::unique_ptr<AST::WhileStatement> Parser::parseWhileStatement() {
    const Token whileTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::WHILE);

    auto condition = parseExpression();
    if (!condition) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    auto body = parseBlockStatement();
    if (!body) return nullptr;

    const uint32_t endIndex = body->sourceEndIndex();
    return std::make_unique<AST::WhileStatement>(std::move(condition), std::move(body),
                                                 whileTok.byteOffsetStart(), endIndex);
}

std::unique_ptr<AST::BreakStatement> Parser::parseBreakStatement() {
    const Token breakTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::BREAK);
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return std::make_unique<AST::BreakStatement>(breakTok.byteOffsetStart(),
                                                 semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ContinueStatement> Parser::parseContinueStatement() {
    const Token continueTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::CONTINUE);
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return std::make_unique<AST::ContinueStatement>(continueTok.byteOffsetStart(),
                                                    semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ReturnStatement> Parser::parseReturnStatement() {
    const Token returnTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::RETURN);

    std::unique_ptr<AST::Expression> returnValue;
    if (peek().kind() != TokenKind::SEMICOLON) {
        returnValue = parseExpression();
        if (!returnValue) return nullptr;
    }

    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

    return std::make_unique<AST::ReturnStatement>(
        std::move(returnValue), returnTok.byteOffsetStart(), semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ExitStatement> Parser::parseExitStatement() {
    const Token exitTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::EXIT);
    auto exitCode = parseExpression();
    if (!exitCode) return nullptr;
    const Token semiTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExitStatement>(std::move(exitCode), exitTok.byteOffsetStart(),
                                                semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::BlockStatement> Parser::parseBlockStatement() {
    const Token lBrace = EXPECT_OR_RETURN_NULLPTR(TokenKind::LEFT_BRACE);

    std::vector<std::unique_ptr<AST::Statement>> statements;
    while (peek().kind() != TokenKind::EOF_ && peek().kind() != TokenKind::RIGHT_BRACE) {
        if (auto stmt = parseStatement()) {
            statements.push_back(std::move(stmt));
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

    return std::make_unique<AST::BlockStatement>(std::move(statements), lBrace.byteOffsetStart(),
                                                 rBrace.byteOffsetEnd());
}

std::unique_ptr<AST::Statement> Parser::parseStatement() {
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

std::unique_ptr<AST::VariableDefinition> Parser::parseFunctionParameter() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isMutable = advanceIf(TokenKind::MUT);

    auto identifier = parseIdentifier();
    if (!identifier) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);

    const auto parsedType = parseTypeSpecifier();
    if (!parsedType) return nullptr;
    const TypeID typeID = typeManager_.createType(*parsedType);

    const uint32_t endIndex = identifier->sourceEndIndex();
    return std::make_unique<AST::VariableDefinition>(std::move(identifier), typeID, isMutable,
                                                     sourceStartIndex, endIndex);
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

    return std::make_unique<ParsedFunctionSignature>(std::move(identifier),
                                                     std::move(parameters.value()),
                                                     typeManager_.createType(returnType.value()));
}

std::unique_ptr<AST::ExternalFunctionDeclaration> Parser::parseExternalFunctionDeclaration() {
    const Token externTok = EXPECT_OR_RETURN_NULLPTR(TokenKind::EXTERN);
    EXPECT_OR_RETURN_NULLPTR(TokenKind::FN);

    const auto signature = parseFunctionSignature();
    if (!signature) return nullptr;

    const Token semi = EXPECT_OR_RETURN_NULLPTR(TokenKind::SEMICOLON);

    return std::make_unique<AST::ExternalFunctionDeclaration>(
        std::move(signature->identifier_), std::move(signature->parameters_),
        signature->returnTypeID_, externTok.byteOffsetStart(), semi.byteOffsetEnd());
}

std::unique_ptr<AST::FunctionDefinition> Parser::parseFunctionDefinition() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isExported = advanceIf(TokenKind::EXPORT);

    EXPECT_OR_RETURN_NULLPTR(TokenKind::FN);

    const auto signature = parseFunctionSignature();
    if (!signature) return nullptr;

    EXPECT_OR_RETURN_NULLPTR(TokenKind::COLON);
    auto body = parseBlockStatement();
    if (!body) return nullptr;

    const uint32_t endIndex = body->sourceEndIndex();
    return std::make_unique<AST::FunctionDefinition>(
        std::move(signature->identifier_), std::move(signature->parameters_),
        signature->returnTypeID_, isExported, std::move(body), sourceStartIndex, endIndex);
}

std::unique_ptr<AST::Program> Parser::parseProgram() {
    auto program = std::make_unique<AST::Program>();

    while (peek().kind() == TokenKind::EXTERN) {
        if (auto externFunction = parseExternalFunctionDeclaration()) {
            program->appendExternFunction(std::move(externFunction));
        } else {
            // Error recovery: skip to the next semicolon
            while (peek().kind() != TokenKind::SEMICOLON && peek().kind() != TokenKind::EOF_) {
                advance();
            }
            advanceIf(TokenKind::SEMICOLON);
        }
    }

    while (peek().kind() != TokenKind::EOF_) {
        if (auto functionDefinition = parseFunctionDefinition()) {
            program->appendFunction(std::move(functionDefinition));

        } else {
            // Error recovery: skip to the next function definition
            while (peek().kind() != TokenKind::FN && peek().kind() != TokenKind::EXPORT &&
                   peek().kind() != TokenKind::EOF_) {
                advance();
            }
        }
    }

    return program;
}
