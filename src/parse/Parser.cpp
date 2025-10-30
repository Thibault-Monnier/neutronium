#include "Parser.hpp"

#include <algorithm>
#include <format>
#include <functional>
#include <memory>
#include <print>
#include <string>

#include "lex/Token.hpp"

std::unique_ptr<AST::Program> Parser::parse() { return parseProgram(); }

void Parser::abort(const std::string& errorMessage) const {
    const Token& token = peek();
    diagnosticsEngine_.reportError(errorMessage, token.byteOffsetStart(), token.byteOffsetEnd());

    diagnosticsEngine_.emitErrors();
    exit(EXIT_FAILURE);
}

const Token& Parser::peek(const int amount) const { return tokens_.at(currentIndex_ + amount); }

const Token& Parser::advance() {
    const Token& token = peek();
    currentIndex_++;
    return token;
}

bool Parser::advanceIf(const TokenKind expected) {
    const Token& token = peek();

    const bool matches = token.kind() == expected;
    if (matches) advance();

    return matches;
}

const Token& Parser::expect(const TokenKind expected) {
    const Token& token = peek();

    if (token.kind() != expected) {
        const std::string errorMessage =
            std::format("Invalid token -> expected {}, got {}", tokenKindToString(expected),
                        tokenKindToString(token.kind()));
        abort(errorMessage);
    }

    return advance();
}

template <typename T>
std::vector<std::unique_ptr<T>> Parser::parseCommaSeparatedList(
    const TokenKind endDelimiter, const std::function<std::unique_ptr<T>()>& parseElement) {
    std::vector<std::unique_ptr<T>> elements;
    while (peek().kind() != endDelimiter) {
        elements.push_back(parseElement());
        if (!advanceIf(TokenKind::COMMA)) break;
    }
    return elements;
}

std::vector<std::unique_ptr<AST::Expression>> Parser::parseExpressionList(
    const TokenKind endDelimiter) {
    return parseCommaSeparatedList<AST::Expression>(endDelimiter,
                                                    [this]() { return parseExpression(); });
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

Type Parser::parseTypeSpecifier() {
    const TokenKind tokenKind = peek().kind();

    if (auto type = tryParsePrimitiveType(tokenKind)) {
        expect(tokenKind);
        return *type;
    }

    if (advanceIf(TokenKind::LEFT_BRACKET)) {
        const TypeID elementTypeID = typeManager_.createType(parseTypeSpecifier());
        expect(TokenKind::SEMICOLON);
        const std::size_t arrayLength = parseNumberLiteral()->value_;
        expect(TokenKind::RIGHT_BRACKET);
        return Type{elementTypeID, arrayLength};
    }

    const std::string errorMessage = std::format("Invalid token -> expected type specifier, got {}",
                                                 tokenKindToString(tokenKind));
    abort(errorMessage);
}

std::unique_ptr<AST::Identifier> Parser::parseIdentifier() {
    const Token& ident = expect(TokenKind::IDENTIFIER);
    return std::make_unique<AST::Identifier>(ident.lexeme(), ident.byteOffsetStart(),
                                             ident.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::NumberLiteral> Parser::parseNumberLiteral() {
    const Token& token = expect(TokenKind::NUMBER_LITERAL);
    return std::make_unique<AST::NumberLiteral>(std::stoll(token.lexeme()), token.byteOffsetStart(),
                                                token.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::ArrayLiteral> Parser::parseArrayLiteral() {
    const Token& lBracket = expect(TokenKind::LEFT_BRACKET);
    auto elements = parseExpressionList(TokenKind::RIGHT_BRACKET);
    const Token& rBracket = expect(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayLiteral>(std::move(elements), lBracket.byteOffsetStart(),
                                               rBracket.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::FunctionCall> Parser::parseFunctionCall() {
    auto callee = parseIdentifier();

    expect(TokenKind::LEFT_PAREN);
    auto arguments = parseExpressionList(TokenKind::RIGHT_PAREN);
    const Token& rParen = expect(TokenKind::RIGHT_PAREN);

    return std::make_unique<AST::FunctionCall>(std::move(callee), std::move(arguments),
                                               callee->sourceStartIndex(), rParen.byteOffsetEnd(),
                                               generateAnyType());
}

std::unique_ptr<AST::ArrayAccess> Parser::parseArrayAccess(std::unique_ptr<AST::Expression> base) {
    expect(TokenKind::LEFT_BRACKET);
    auto index = parseExpression();
    const Token& rBracket = expect(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayAccess>(std::move(base), std::move(index),
                                              base->sourceStartIndex(), rBracket.byteOffsetEnd(),
                                              generateAnyType());
}

std::unique_ptr<AST::Expression> Parser::parsePrimaryExpression() {
    const Token& token = peek();

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
            if (peek(1).kind() == TokenKind::LEFT_PAREN) {
                return parseFunctionCall();
            }
            return parseIdentifier();

        case TokenKind::LEFT_PAREN: {
            expect(TokenKind::LEFT_PAREN);
            auto inner = parseExpression();
            expect(TokenKind::RIGHT_PAREN);
            return inner;
        }

        default: {
            const std::string errorMessage =
                std::format("Invalid token at beginning of primary expression -> got {}",
                            tokenKindToString(token.kind()));
            abort(errorMessage);
        }
    }
}

std::unique_ptr<AST::Expression> Parser::parsePostfixExpression() {
    auto postfixExpr = parsePrimaryExpression();

    while (true) {
        const Token& token = peek();
        if (token.kind() == TokenKind::LEFT_BRACKET) {
            postfixExpr = parseArrayAccess(std::move(postfixExpr));
        } else {
            break;
        }
    }

    return postfixExpr;
}

std::unique_ptr<AST::Expression> Parser::parseUnaryExpression() {
    const Token& token = peek();

    const AST::Operator op = AST::tokenKindToOperator(token.kind());
    if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
        op == AST::Operator::LOGICAL_NOT) {
        expect(token.kind());
        auto operand = parsePostfixExpression();
        return std::make_unique<AST::UnaryExpression>(op, std::move(operand),
                                                      token.byteOffsetStart(),
                                                      operand->sourceEndIndex(), generateAnyType());
    }

    return parsePostfixExpression();
}

std::unique_ptr<AST::Expression> Parser::parseBinaryExpression(
    const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
    const std::initializer_list<AST::Operator> allowedOps, const bool allowMultiple) {
    auto left = parseOperand();
    while (true) {
        const AST::Operator op = AST::tokenKindToOperator(peek().kind());
        if (std::ranges::find(allowedOps, op) == allowedOps.end()) break;

        advance();
        auto right = parseOperand();
        left = std::make_unique<AST::BinaryExpression>(std::move(left), op, std::move(right),
                                                       left->sourceStartIndex(),
                                                       right->sourceEndIndex(), generateAnyType());
        if (!allowMultiple) break;
    }

    return left;
}

std::unique_ptr<AST::Expression> Parser::parseMultiplicativeExpression() {
    return parseBinaryExpression([this]() { return parseUnaryExpression(); },
                                 {AST::Operator::MULTIPLY, AST::Operator::DIVIDE}, true);
}

std::unique_ptr<AST::Expression> Parser::parseAdditiveExpression() {
    return parseBinaryExpression([this]() { return parseMultiplicativeExpression(); },
                                 {AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
}

std::unique_ptr<AST::Expression> Parser::parseComparisonExpression() {
    return parseBinaryExpression(
        [this]() { return parseAdditiveExpression(); },
        {AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
         AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
         AST::Operator::GREATER_THAN_OR_EQUAL},
        false);
}

std::unique_ptr<AST::Expression> Parser::parseExpression() { return parseComparisonExpression(); }

std::unique_ptr<AST::ExpressionStatement> Parser::parseExpressionStatement() {
    auto expression = parseExpression();
    const Token& semi = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExpressionStatement>(
        std::move(expression), expression->sourceStartIndex(), semi.byteOffsetEnd());
}

std::unique_ptr<AST::VariableDefinition> Parser::parseVariableDefinition() {
    const Token& let = expect(TokenKind::LET);

    const bool isMutable = advanceIf(TokenKind::MUT);

    auto identifier = parseIdentifier();

    const Type type = advanceIf(TokenKind::COLON) ? parseTypeSpecifier() : Type::anyFamilyType();
    const TypeID typeID = typeManager_.createType(type);

    expect(TokenKind::EQUAL);
    auto value = parseExpression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::VariableDefinition>(std::move(identifier), typeID, isMutable,
                                                     std::move(value), let.byteOffsetStart(),
                                                     semi.byteOffsetEnd());
}

std::unique_ptr<AST::Assignment> Parser::parseAssignment() {
    auto left = parseExpression();

    const Token& operatorToken = advance();
    const AST::Operator op = AST::tokenKindToOperator(operatorToken.kind());

    auto right = parseExpression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::Assignment>(std::move(left), op, std::move(right),
                                             left->sourceStartIndex(), semi.byteOffsetEnd());
}

std::unique_ptr<AST::IfStatement> Parser::constructIfStatement(
    std::unique_ptr<AST::Expression> condition, std::unique_ptr<AST::BlockStatement> body,
    std::unique_ptr<AST::BlockStatement> elseClause, uint32_t startIndex) {
    const uint32_t endIndex = elseClause ? elseClause->sourceEndIndex() : body->sourceEndIndex();
    return std::make_unique<AST::IfStatement>(std::move(condition), std::move(body),
                                              std::move(elseClause), startIndex, endIndex);
}

std::unique_ptr<AST::BlockStatement> Parser::parseElseClause() {
    if (advanceIf(TokenKind::ELSE)) {
        expect(TokenKind::COLON);
        return parseBlockStatement();
    }

    // ELIF case
    const Token& elifTok = expect(TokenKind::ELIF);

    auto elifCondition = parseExpression();
    expect(TokenKind::COLON);
    auto elifBody = parseBlockStatement();

    std::unique_ptr<AST::BlockStatement> elseClause;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        elseClause = parseElseClause();
    }

    auto elifStmt = constructIfStatement(std::move(elifCondition), std::move(elifBody),
                                         std::move(elseClause), elifTok.byteOffsetStart());

    std::vector<std::unique_ptr<AST::Statement>> statements;
    statements.push_back(std::move(elifStmt));
    return std::make_unique<AST::BlockStatement>(std::move(statements),
                                                 statements.front()->sourceStartIndex(),
                                                 statements.back()->sourceEndIndex());
}

std::unique_ptr<AST::IfStatement> Parser::parseIfStatement() {
    const Token& ifTok = expect(TokenKind::IF);
    auto condition = parseExpression();
    expect(TokenKind::COLON);
    auto body = parseBlockStatement();

    std::unique_ptr<AST::BlockStatement> elseClause;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        elseClause = parseElseClause();
    }

    return constructIfStatement(std::move(condition), std::move(body), std::move(elseClause),
                                ifTok.byteOffsetStart());
}

std::unique_ptr<AST::WhileStatement> Parser::parseWhileStatement() {
    const Token& whileTok = expect(TokenKind::WHILE);
    auto condition = parseExpression();
    expect(TokenKind::COLON);
    auto body = parseBlockStatement();
    return std::make_unique<AST::WhileStatement>(
        std::move(condition), std::move(body), whileTok.byteOffsetStart(), body->sourceEndIndex());
}

std::unique_ptr<AST::BreakStatement> Parser::parseBreakStatement() {
    const Token& breakTok = expect(TokenKind::BREAK);
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::BreakStatement>(breakTok.byteOffsetStart(),
                                                 semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ContinueStatement> Parser::parseContinueStatement() {
    const Token& continueTok = expect(TokenKind::CONTINUE);
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ContinueStatement>(continueTok.byteOffsetStart(),
                                                    semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ReturnStatement> Parser::parseReturnStatement() {
    const Token& returnTok = expect(TokenKind::RETURN);
    auto returnValue = parseExpression();
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ReturnStatement>(
        std::move(returnValue), returnTok.byteOffsetStart(), semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::ExitStatement> Parser::parseExitStatement() {
    const Token& exitTok = expect(TokenKind::EXIT);
    auto exitCode = parseExpression();
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExitStatement>(std::move(exitCode), exitTok.byteOffsetStart(),
                                                semiTok.byteOffsetEnd());
}

std::unique_ptr<AST::BlockStatement> Parser::parseBlockStatement() {
    const Token& lBrace = expect(TokenKind::LEFT_BRACE);

    std::vector<std::unique_ptr<AST::Statement>> statements;
    while (peek().kind() != TokenKind::RIGHT_BRACE) {
        statements.push_back(parseStatement());
    }

    const Token& rBrace = expect(TokenKind::RIGHT_BRACE);

    return std::make_unique<AST::BlockStatement>(std::move(statements), lBrace.byteOffsetStart(),
                                                 rBrace.byteOffsetEnd());
}

bool Parser::assignmentOperatorAhead() const {
    for (int i = 0; peek(i).kind() != TokenKind::EOF_ && peek(i).kind() != TokenKind::SEMICOLON;
         i++) {
        if (AST::isAssignmentOperator(AST::tokenKindToOperator(peek(i).kind()))) {
            return true;
        }
    }
    return false;
}

std::unique_ptr<AST::Statement> Parser::parseStatement() {
    const TokenKind tokenKind = peek().kind();

    if (tokenKind == TokenKind::LET) return parseVariableDefinition();
    if (tokenKind == TokenKind::IDENTIFIER && assignmentOperatorAhead()) return parseAssignment();
    if (tokenKind == TokenKind::IF) return parseIfStatement();
    if (tokenKind == TokenKind::WHILE) return parseWhileStatement();
    if (tokenKind == TokenKind::BREAK) return parseBreakStatement();
    if (tokenKind == TokenKind::CONTINUE) return parseContinueStatement();
    if (tokenKind == TokenKind::RETURN) return parseReturnStatement();
    if (tokenKind == TokenKind::EXIT) return parseExitStatement();
    if (tokenKind == TokenKind::LEFT_BRACE) return parseBlockStatement();
    return parseExpressionStatement();
}

std::unique_ptr<AST::VariableDefinition> Parser::parseFunctionParameter() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isMutable = advanceIf(TokenKind::MUT);

    auto identifier = parseIdentifier();

    expect(TokenKind::COLON);
    const Type type = parseTypeSpecifier();
    const TypeID typeID = typeManager_.createType(type);

    return std::make_unique<AST::VariableDefinition>(
        std::move(identifier), typeID, isMutable, sourceStartIndex, identifier->sourceEndIndex());
}

ParsedFunctionSignature Parser::parseFunctionSignature() {
    auto identifier = parseIdentifier();

    expect(TokenKind::LEFT_PAREN);
    auto parameters = parseCommaSeparatedList<AST::VariableDefinition>(
        TokenKind::RIGHT_PAREN, [this]() { return parseFunctionParameter(); });
    expect(TokenKind::RIGHT_PAREN);

    const Type returnType =
        advanceIf(TokenKind::RIGHT_ARROW) ? parseTypeSpecifier() : Type::voidType();

    return {.identifier_ = std::move(identifier),
            .parameters_ = std::move(parameters),
            .returnTypeID_ = typeManager_.createType(returnType)};
}

std::unique_ptr<AST::ExternalFunctionDeclaration> Parser::parseExternalFunctionDeclaration() {
    const Token& externTok = expect(TokenKind::EXTERN);
    expect(TokenKind::FN);

    ParsedFunctionSignature signature = parseFunctionSignature();

    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::ExternalFunctionDeclaration>(
        std::move(signature.identifier_), std::move(signature.parameters_), signature.returnTypeID_,
        externTok.byteOffsetStart(), semi.byteOffsetEnd());
}

std::unique_ptr<AST::FunctionDefinition> Parser::parseFunctionDefinition() {
    const uint32_t sourceStartIndex = peek().byteOffsetStart();

    const bool isExported = advanceIf(TokenKind::EXPORT);

    expect(TokenKind::FN);

    ParsedFunctionSignature signature = parseFunctionSignature();

    expect(TokenKind::COLON);
    auto body = parseBlockStatement();
    return std::make_unique<AST::FunctionDefinition>(
        std::move(signature.identifier_), std::move(signature.parameters_), signature.returnTypeID_,
        isExported, std::move(body), sourceStartIndex, body->sourceEndIndex());
}

std::unique_ptr<AST::Program> Parser::parseProgram() {
    auto program = std::make_unique<AST::Program>();

    while (peek().kind() == TokenKind::EXTERN) {
        auto externFunction = parseExternalFunctionDeclaration();
        program->appendExternFunction(std::move(externFunction));
    }
    while (peek().kind() != TokenKind::EOF_) {
        if (peek().kind() == TokenKind::FN || peek().kind() == TokenKind::EXPORT) {
            auto functionDefinition = parseFunctionDefinition();
            program->appendFunction(std::move(functionDefinition));
        } else {
            const std::string errorMessage =
                std::format("Invalid token -> expected function definition, got {}",
                            tokenKindToString(peek().kind()));
            abort(errorMessage);
        }
    }

    return program;
}
