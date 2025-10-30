#include "Parser.hpp"

#include <format>
#include <functional>
#include <memory>
#include <print>
#include <set>
#include <string>

#include "lex/Token.hpp"

std::unique_ptr<AST::Program> Parser::parse() {
    auto program = parseProgram();
    return program;
}

void Parser::abort(const std::string& errorMessage) const {
    const Token& token = peek();
    diagnosticsEngine_.reportError(errorMessage, token.byteOffsetStart(), token.byteOffsetEnd());

    diagnosticsEngine_.emitErrors();
    exit(EXIT_FAILURE);
}

const Token& Parser::peek(const int amount) const { return tokens_.at(currentIndex_ + amount); }

const Token& Parser::expect(const TokenKind expected) {
    const Token& token = peek();

    if (token.kind() != expected) {
        const std::string errorMessage =
            std::format("Invalid token -> expected {}, got {}", tokenKindToString(expected),
                        tokenKindToString(token.kind()));
        abort(errorMessage);
    }

    currentIndex_++;
    return token;
}

Type Parser::parseTypeSpecifier() {
    const TokenKind tokenKind = peek().kind();

    switch (tokenKind) {
        case TokenKind::INT:
            expect(tokenKind);
            return Primitive::Kind::INT;
        case TokenKind::INT8:
            expect(tokenKind);
            return Primitive::Kind::INT8;
        case TokenKind::INT16:
            expect(tokenKind);
            return Primitive::Kind::INT16;
        case TokenKind::INT32:
            expect(tokenKind);
            return Primitive::Kind::INT32;
        case TokenKind::INT64:
            expect(tokenKind);
            return Primitive::Kind::INT64;
        case TokenKind::BOOL:
            expect(tokenKind);
            return Primitive::Kind::BOOL;
        case TokenKind::LEFT_BRACKET: {
            expect(tokenKind);
            const TypeID elementTypeID = typeManager_.createType(parseTypeSpecifier());
            expect(TokenKind::SEMICOLON);
            const std::size_t arrayLength = parseNumberLiteral()->value_;
            expect(TokenKind::RIGHT_BRACKET);
            return Type{elementTypeID, arrayLength};
        }

        default: {
            const std::string errorMessage = std::format(
                "Invalid token -> expected type specifier, got {}", tokenKindToString(tokenKind));
            abort(errorMessage);
        }
    }
}

std::unique_ptr<AST::NumberLiteral> Parser::parseNumberLiteral() {
    const Token& token = expect(TokenKind::NUMBER_LITERAL);
    return std::make_unique<AST::NumberLiteral>(std::stoll(token.lexeme()), token.byteOffsetStart(),
                                                token.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::ArrayLiteral> Parser::parseArrayLiteral() {
    std::vector<std::unique_ptr<AST::Expression>> elements;

    const Token& lBracket = expect(TokenKind::LEFT_BRACKET);
    while (peek().kind() != TokenKind::RIGHT_BRACKET) {
        elements.push_back(parseExpression());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        } else {
            break;
        }
    }
    const Token& rBracket = expect(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayLiteral>(std::move(elements), lBracket.byteOffsetStart(),
                                               rBracket.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::Identifier> Parser::parseIdentifier() {
    const Token& ident = expect(TokenKind::IDENTIFIER);
    return std::make_unique<AST::Identifier>(ident.lexeme(), ident.byteOffsetStart(),
                                             ident.byteOffsetEnd(), generateAnyType());
}

std::unique_ptr<AST::FunctionCall> Parser::parseFunctionCall() {
    auto callee = parseIdentifier();

    std::vector<std::unique_ptr<AST::Expression>> arguments;

    expect(TokenKind::LEFT_PAREN);
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        arguments.push_back(parseExpression());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        } else {
            break;
        }
    }
    const Token& rParen = expect(TokenKind::RIGHT_PAREN);

    return std::make_unique<AST::FunctionCall>(std::move(callee), std::move(arguments),
                                               callee->sourceStartIndex(), rParen.byteOffsetEnd(),
                                               generateAnyType());
}

std::unique_ptr<AST::ArrayAccess> Parser::parseArrayAccess(std::unique_ptr<AST::Expression>& base) {
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
            expect(TokenKind::TRUE);
            return std::make_unique<AST::BooleanLiteral>(true, token.byteOffsetStart(),
                                                         token.byteOffsetEnd(), generateAnyType());

        case TokenKind::FALSE:
            expect(TokenKind::FALSE);
            return std::make_unique<AST::BooleanLiteral>(false, token.byteOffsetStart(),
                                                         token.byteOffsetEnd(), generateAnyType());

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

        default:
            const std::string errorMessage =
                std::format("Invalid token at beginning of primary expression -> got {}",
                            tokenKindToString(token.kind()));
            abort(errorMessage);
    }
}

std::unique_ptr<AST::Expression> Parser::parsePostfixExpression() {
    auto postfixExpr = parsePrimaryExpression();

    while (true) {
        const Token& token = peek();
        if (token.kind() == TokenKind::LEFT_BRACKET) {
            postfixExpr = parseArrayAccess(postfixExpr);
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
    const std::set<AST::Operator>& allowedOps, const bool allowMultiple) {
    auto left = parseOperand();
    while (true) {
        const Token& token = peek();
        const AST::Operator op = AST::tokenKindToOperator(token.kind());
        if (allowedOps.contains(op)) {
            expect(token.kind());
            auto right = parseOperand();
            left = std::make_unique<AST::BinaryExpression>(
                std::move(left), op, std::move(right), left->sourceStartIndex(),
                right->sourceEndIndex(), generateAnyType());

            if (!allowMultiple) {
                break;
            }
        } else {
            break;
        }
    }

    return left;
}

std::unique_ptr<AST::Expression> Parser::parseMultiplicativeExpression() {
    return parseBinaryExpression([this]() { return parseUnaryExpression(); },
                                 std::set{AST::Operator::MULTIPLY, AST::Operator::DIVIDE}, true);
}

std::unique_ptr<AST::Expression> Parser::parseAdditiveExpression() {
    return parseBinaryExpression([this]() { return parseMultiplicativeExpression(); },
                                 std::set{AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
}

std::unique_ptr<AST::Expression> Parser::parseComparisonExpression() {
    return parseBinaryExpression(
        [this]() { return parseAdditiveExpression(); },
        std::set{AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
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

    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        expect(TokenKind::MUT);
        isMutable = true;
    }

    auto identifier = parseIdentifier();

    Type type = Type::anyFamilyType();
    if (peek().kind() == TokenKind::COLON) {
        expect(TokenKind::COLON);
        type = parseTypeSpecifier();
    }
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
    const AST::Operator op = AST::tokenKindToOperator(expect(peek().kind()).kind());
    auto right = parseExpression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::Assignment>(std::move(left), op, std::move(right),
                                             left->sourceStartIndex(), semi.byteOffsetEnd());
}

std::unique_ptr<AST::BlockStatement> Parser::parseElseClause() {
    if (peek().kind() == TokenKind::ELIF) {
        const Token& elifTok = expect(TokenKind::ELIF);

        auto elifCondition = parseExpression();
        expect(TokenKind::COLON);
        auto elifBody = parseBlockStatement();

        std::unique_ptr<AST::IfStatement> elifStmt;
        if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
            auto elseClause = parseElseClause();
            elifStmt = std::make_unique<AST::IfStatement>(
                std::move(elifCondition), std::move(elifBody), std::move(elseClause),
                elifTok.byteOffsetStart(), elseClause->sourceEndIndex());
        } else {
            elifStmt = std::make_unique<AST::IfStatement>(
                std::move(elifCondition), std::move(elifBody), elifTok.byteOffsetStart(),
                elifBody->sourceEndIndex());
        }

        std::vector<std::unique_ptr<AST::Statement>> statements;
        statements.push_back(std::move(elifStmt));
        auto block = std::make_unique<AST::BlockStatement>(std::move(statements),
                                                           statements.front()->sourceStartIndex(),
                                                           statements.back()->sourceEndIndex());
        return block;
    } else if (peek().kind() == TokenKind::ELSE) {
        expect(TokenKind::ELSE);
        expect(TokenKind::COLON);
        auto elseBody = parseBlockStatement();
        return elseBody;
    }

    std::unreachable();
}

std::unique_ptr<AST::IfStatement> Parser::parseIfStatement() {
    const Token& ifTok = expect(TokenKind::IF);
    auto condition = parseExpression();
    expect(TokenKind::COLON);

    auto body = parseBlockStatement();

    std::unique_ptr<AST::IfStatement> ifStmt;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        auto elseClause = parseElseClause();
        ifStmt = std::make_unique<AST::IfStatement>(std::move(condition), std::move(body),
                                                    std::move(elseClause), ifTok.byteOffsetStart(),
                                                    elseClause->sourceEndIndex());
    } else {
        ifStmt = std::make_unique<AST::IfStatement>(
            std::move(condition), std::move(body), ifTok.byteOffsetStart(), body->sourceEndIndex());
    }

    return ifStmt;
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

std::unique_ptr<AST::Statement> Parser::parseStatement() {
    const TokenKind tokenKind = peek().kind();

    if (tokenKind == TokenKind::LET) return parseVariableDefinition();
    if (tokenKind == TokenKind::IDENTIFIER)
        for (int i = 0; peek(i).kind() != TokenKind::EOF_ && peek(i).kind() != TokenKind::SEMICOLON;
             i++)
            if (AST::isAssignmentOperator(AST::tokenKindToOperator(peek(i).kind())))
                return parseAssignment();
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

    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        expect(TokenKind::MUT);
        isMutable = true;
    }

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
    std::vector<std::unique_ptr<AST::VariableDefinition>> parameters;
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        parameters.push_back(parseFunctionParameter());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        }
    }
    expect(TokenKind::RIGHT_PAREN);

    Type returnType = Primitive::Kind::VOID;
    if (peek().kind() == TokenKind::RIGHT_ARROW) {
        expect(TokenKind::RIGHT_ARROW);
        returnType = parseTypeSpecifier();
    }

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

    bool isExported = false;
    if (peek().kind() == TokenKind::EXPORT) {
        expect(TokenKind::EXPORT);
        isExported = true;
    }

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
