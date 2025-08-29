#include "parsing/parser.hpp"

#include <format>
#include <functional>
#include <memory>
#include <print>
#include <set>
#include <string>

#include "lexing/token.hpp"

std::unique_ptr<AST::Program> Parser::parse() {
    auto program = parse_program();
    return program;
}

void Parser::abort(const std::string& errorMessage) const {
    const Token& token = peek();
    diagnosticsEngine_.report_error(errorMessage, token.byte_offset_start(),
                                    token.byte_offset_end());

    diagnosticsEngine_.emit_errors();
    exit(EXIT_FAILURE);
}

const Token& Parser::peek(const int amount) const { return tokens_.at(currentIndex_ + amount); }

const Token& Parser::expect(const TokenKind expected) {
    const Token& token = peek();

    if (token.kind() != expected) {
        const std::string errorMessage =
            std::format("Invalid token -> expected {}, got {}", token_kind_to_string(expected),
                        token_kind_to_string(token.kind()));
        abort(errorMessage);
    }

    currentIndex_++;
    return token;
}

Type Parser::parse_type_specifier() {
    const TokenKind tokenKind = peek().kind();
    expect(tokenKind);

    switch (tokenKind) {
        case TokenKind::INT:
            return PrimitiveType::INTEGER;
        case TokenKind::BOOL:
            return PrimitiveType::BOOLEAN;
        case TokenKind::LEFT_BRACKET: {
            const Type elementType = parse_type_specifier();
            expect(TokenKind::SEMICOLON);
            const std::size_t arrayLength = parse_number_literal()->value_;
            expect(TokenKind::RIGHT_BRACKET);
            return Type{elementType, arrayLength};
        }

        default: {
            const std::string errorMessage =
                std::format("Invalid token -> expected type specifier, got {}",
                            token_kind_to_string(tokenKind));
            abort(errorMessage);
        }
    }
}

std::unique_ptr<AST::NumberLiteral> Parser::parse_number_literal() {
    const Token& token = expect(TokenKind::NUMBER_LITERAL);
    return std::make_unique<AST::NumberLiteral>(std::stoll(token.lexeme()),
                                                token.byte_offset_start(), token.byte_offset_end());
}

std::unique_ptr<AST::ArrayLiteral> Parser::parse_array_literal() {
    std::vector<std::unique_ptr<AST::Expression>> elements;

    const Token& lBracket = expect(TokenKind::LEFT_BRACKET);
    while (peek().kind() != TokenKind::RIGHT_BRACKET) {
        elements.push_back(parse_expression());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        } else {
            break;
        }
    }
    const Token& rBracket = expect(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayLiteral>(std::move(elements), lBracket.byte_offset_start(),
                                               rBracket.byte_offset_end());
}

std::unique_ptr<AST::Identifier> Parser::parse_identifier() {
    const Token& ident = expect(TokenKind::IDENTIFIER);
    return std::make_unique<AST::Identifier>(ident.lexeme(), ident.byte_offset_start(),
                                             ident.byte_offset_end());
}

std::unique_ptr<AST::FunctionCall> Parser::parse_function_call() {
    auto callee = parse_identifier();

    std::vector<std::unique_ptr<AST::Expression>> arguments;

    expect(TokenKind::LEFT_PAREN);
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        arguments.push_back(parse_expression());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        } else {
            break;
        }
    }
    const Token& rParen = expect(TokenKind::RIGHT_PAREN);

    return std::make_unique<AST::FunctionCall>(std::move(callee), std::move(arguments),
                                               callee->source_start_index(),
                                               rParen.byte_offset_end());
}

std::unique_ptr<AST::ArrayAccess> Parser::parse_array_access(
    std::unique_ptr<AST::Expression>& base) {
    const Token& lBracket = expect(TokenKind::LEFT_BRACKET);
    auto index = parse_expression();
    const Token& rBracket = expect(TokenKind::RIGHT_BRACKET);

    return std::make_unique<AST::ArrayAccess>(std::move(base), std::move(index),
                                              lBracket.byte_offset_start(),
                                              rBracket.byte_offset_end());
}

std::unique_ptr<AST::Expression> Parser::parse_primary_expression() {
    const Token& token = peek();

    switch (token.kind()) {
        case TokenKind::NUMBER_LITERAL:
            return parse_number_literal();

        case TokenKind::LEFT_BRACKET:
            return parse_array_literal();

        case TokenKind::TRUE:
            expect(TokenKind::TRUE);
            return std::make_unique<AST::BooleanLiteral>(true, token.byte_offset_start(),
                                                         token.byte_offset_end());

        case TokenKind::FALSE:
            expect(TokenKind::FALSE);
            return std::make_unique<AST::BooleanLiteral>(false, token.byte_offset_start(),
                                                         token.byte_offset_end());

        case TokenKind::IDENTIFIER:
            if (peek(1).kind() == TokenKind::LEFT_PAREN) {
                return parse_function_call();
            }
            return parse_identifier();

        case TokenKind::LEFT_PAREN: {
            expect(TokenKind::LEFT_PAREN);
            auto inner = parse_expression();
            expect(TokenKind::RIGHT_PAREN);
            return inner;
        }

        default:
            const std::string errorMessage =
                std::format("Invalid token at beginning of primary expression -> got {}",
                            token_kind_to_string(token.kind()));
            abort(errorMessage);
    }
}

std::unique_ptr<AST::Expression> Parser::parse_postfix_expression() {
    auto postfixExpr = parse_primary_expression();

    while (true) {
        const Token& token = peek();
        if (token.kind() == TokenKind::LEFT_BRACKET) {
            postfixExpr = parse_array_access(postfixExpr);
        } else {
            break;
        }
    }

    return postfixExpr;
}

std::unique_ptr<AST::Expression> Parser::parse_unary_expression() {
    const Token& token = peek();

    const AST::Operator op = AST::token_kind_to_operator(token.kind());
    if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
        op == AST::Operator::LOGICAL_NOT) {
        expect(token.kind());
        auto operand = parse_postfix_expression();
        return std::make_unique<AST::UnaryExpression>(
            op, std::move(operand), token.byte_offset_start(), operand->source_end_index());
    }

    return parse_postfix_expression();
}

std::unique_ptr<AST::Expression> Parser::parse_binary_expression(
    const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
    const std::set<AST::Operator>& allowedOps, const bool allowMultiple) {
    auto left = parseOperand();
    while (true) {
        const Token& token = peek();
        const AST::Operator op = AST::token_kind_to_operator(token.kind());
        if (allowedOps.contains(op)) {
            expect(token.kind());
            auto right = parseOperand();
            left = std::make_unique<AST::BinaryExpression>(std::move(left), op, std::move(right),
                                                           left->source_start_index(),
                                                           right->source_end_index());

            if (!allowMultiple) {
                break;
            }
        } else {
            break;
        }
    }

    return left;
}

std::unique_ptr<AST::Expression> Parser::parse_multiplicative_expression() {
    return parse_binary_expression([this]() { return parse_unary_expression(); },
                                   std::set{AST::Operator::MULTIPLY, AST::Operator::DIVIDE}, true);
}

std::unique_ptr<AST::Expression> Parser::parse_additive_expression() {
    return parse_binary_expression([this]() { return parse_multiplicative_expression(); },
                                   std::set{AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
}

std::unique_ptr<AST::Expression> Parser::parse_comparison_expression() {
    return parse_binary_expression(
        [this]() { return parse_additive_expression(); },
        std::set{AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
                 AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
                 AST::Operator::GREATER_THAN_OR_EQUAL},
        false);
}

std::unique_ptr<AST::Expression> Parser::parse_expression() {
    return parse_comparison_expression();
}

std::unique_ptr<AST::ExpressionStatement> Parser::parse_expression_statement() {
    auto expression = parse_expression();
    const Token& semi = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExpressionStatement>(
        std::move(expression), expression->source_start_index(), semi.byte_offset_end());
}

std::unique_ptr<AST::VariableDefinition> Parser::parse_variable_definition() {
    const Token& let = expect(TokenKind::LET);

    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        expect(TokenKind::MUT);
        isMutable = true;
    }

    auto identifier = parse_identifier();

    Type type = PrimitiveType::ANY;
    if (peek().kind() == TokenKind::COLON) {
        expect(TokenKind::COLON);
        type = parse_type_specifier();
    }

    expect(TokenKind::EQUAL);
    auto value = parse_expression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::VariableDefinition>(std::move(identifier), type, isMutable,
                                                     std::move(value), let.byte_offset_start(),
                                                     semi.byte_offset_end());
}

std::unique_ptr<AST::Assignment> Parser::parse_assignment() {
    auto left = parse_expression();
    const AST::Operator op = AST::token_kind_to_operator(expect(peek().kind()).kind());
    auto right = parse_expression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::Assignment>(std::move(left), op, std::move(right),
                                             left->source_start_index(), semi.byte_offset_end());
}

std::unique_ptr<AST::BlockStatement> Parser::parse_else_clause() {
    if (peek().kind() == TokenKind::ELIF) {
        const Token& elifTok = expect(TokenKind::ELIF);

        auto elifCondition = parse_expression();
        expect(TokenKind::COLON);
        auto elifBody = parse_block_statement();

        std::unique_ptr<AST::IfStatement> elifStmt;
        if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
            auto elseClause = parse_else_clause();
            elifStmt = std::make_unique<AST::IfStatement>(
                std::move(elifCondition), std::move(elifBody), std::move(elseClause),
                elifTok.byte_offset_start(), elseClause->source_end_index());
        } else {
            elifStmt = std::make_unique<AST::IfStatement>(
                std::move(elifCondition), std::move(elifBody), elifTok.byte_offset_start(),
                elifBody->source_end_index());
        }

        auto block = std::make_unique<AST::BlockStatement>(elifStmt->source_start_index(),
                                                           elifStmt->source_start_index());
        block->append_statement(std::move(elifStmt));
        return block;
    } else if (peek().kind() == TokenKind::ELSE) {
        expect(TokenKind::ELSE);
        expect(TokenKind::COLON);
        auto elseBody = parse_block_statement();
        return elseBody;
    }

    std::unreachable();
}

std::unique_ptr<AST::IfStatement> Parser::parse_if_statement() {
    const Token& ifTok = expect(TokenKind::IF);
    auto condition = parse_expression();
    expect(TokenKind::COLON);

    auto body = parse_block_statement();

    std::unique_ptr<AST::IfStatement> ifStmt;
    if (peek().kind() == TokenKind::ELIF || peek().kind() == TokenKind::ELSE) {
        auto elseClause = parse_else_clause();
        ifStmt = std::make_unique<AST::IfStatement>(
            std::move(condition), std::move(body), std::move(elseClause), ifTok.byte_offset_start(),
            elseClause->source_end_index());
    } else {
        ifStmt =
            std::make_unique<AST::IfStatement>(std::move(condition), std::move(body),
                                               ifTok.byte_offset_start(), body->source_end_index());
    }

    return ifStmt;
}

std::unique_ptr<AST::WhileStatement> Parser::parse_while_statement() {
    const Token& whileTok = expect(TokenKind::WHILE);
    auto condition = parse_expression();
    expect(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::WhileStatement>(std::move(condition), std::move(body),
                                                 whileTok.byte_offset_start(),
                                                 body->source_end_index());
}

std::unique_ptr<AST::BreakStatement> Parser::parse_break_statement() {
    const Token& breakTok = expect(TokenKind::BREAK);
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::BreakStatement>(breakTok.byte_offset_start(),
                                                 semiTok.byte_offset_end());
}

std::unique_ptr<AST::ContinueStatement> Parser::parse_continue_statement() {
    const Token& continueTok = expect(TokenKind::CONTINUE);
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ContinueStatement>(continueTok.byte_offset_start(),
                                                    semiTok.byte_offset_end());
}

std::unique_ptr<AST::ReturnStatement> Parser::parse_return_statement() {
    const Token& returnTok = expect(TokenKind::RETURN);
    auto returnValue = parse_expression();
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ReturnStatement>(
        std::move(returnValue), returnTok.byte_offset_start(), semiTok.byte_offset_end());
}

std::unique_ptr<AST::ExitStatement> Parser::parse_exit_statement() {
    const Token& exitTok = expect(TokenKind::EXIT);
    auto exitCode = parse_expression();
    const Token& semiTok = expect(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExitStatement>(std::move(exitCode), exitTok.byte_offset_start(),
                                                semiTok.byte_offset_end());
}

std::unique_ptr<AST::BlockStatement> Parser::parse_block_statement() {
    const Token& lBrace = expect(TokenKind::LEFT_BRACE);
    auto block =
        std::make_unique<AST::BlockStatement>(lBrace.byte_offset_start(), lBrace.byte_offset_end());
    while (peek().kind() != TokenKind::RIGHT_BRACE) {
        block->append_statement(parse_statement());
    }
    expect(TokenKind::RIGHT_BRACE);
    return block;
}

std::unique_ptr<AST::Statement> Parser::parse_statement() {
    const TokenKind tokenKind = peek().kind();

    if (tokenKind == TokenKind::LET) return parse_variable_definition();
    if (tokenKind == TokenKind::IDENTIFIER)
        for (int i = 0; peek(i).kind() != TokenKind::EOF_ && peek(i).kind() != TokenKind::SEMICOLON;
             i++)
            if (AST::is_assignment_operator(AST::token_kind_to_operator(peek(i).kind())))
                return parse_assignment();
    if (tokenKind == TokenKind::IF) return parse_if_statement();
    if (tokenKind == TokenKind::WHILE) return parse_while_statement();
    if (tokenKind == TokenKind::BREAK) return parse_break_statement();
    if (tokenKind == TokenKind::CONTINUE) return parse_continue_statement();
    if (tokenKind == TokenKind::RETURN) return parse_return_statement();
    if (tokenKind == TokenKind::EXIT) return parse_exit_statement();
    if (tokenKind == TokenKind::LEFT_BRACE) return parse_block_statement();
    return parse_expression_statement();
}

std::unique_ptr<AST::VariableDefinition> Parser::parse_function_parameter() {
    const uint32_t sourceStartIndex = peek().byte_offset_start();

    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        expect(TokenKind::MUT);
        isMutable = true;
    }

    auto identifier = parse_identifier();

    expect(TokenKind::COLON);
    const Type type = parse_type_specifier();

    return std::make_unique<AST::VariableDefinition>(
        std::move(identifier), type, isMutable, sourceStartIndex, identifier->source_end_index());
}

ParsedFunctionSignature Parser::parse_function_signature() {
    auto identifier = parse_identifier();

    expect(TokenKind::LEFT_PAREN);
    std::vector<std::unique_ptr<AST::VariableDefinition>> parameters;
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        parameters.push_back(parse_function_parameter());
        if (peek().kind() == TokenKind::COMMA) {
            expect(TokenKind::COMMA);
        }
    }
    expect(TokenKind::RIGHT_PAREN);

    Type returnType = PrimitiveType::VOID;
    if (peek().kind() == TokenKind::RIGHT_ARROW) {
        expect(TokenKind::RIGHT_ARROW);
        returnType = parse_type_specifier();
    }

    return {.identifier_ = std::move(identifier),
            .parameters_ = std::move(parameters),
            .returnType_ = returnType};
}

std::unique_ptr<AST::ExternalFunctionDeclaration> Parser::parse_external_function_declaration() {
    const Token& externTok = expect(TokenKind::EXTERN);
    expect(TokenKind::FN);

    ParsedFunctionSignature signature = parse_function_signature();

    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::ExternalFunctionDeclaration>(
        std::move(signature.identifier_), std::move(signature.parameters_), signature.returnType_,
        externTok.byte_offset_start(), semi.byte_offset_end());
}

std::unique_ptr<AST::FunctionDefinition> Parser::parse_function_definition() {
    const uint32_t sourceStartIndex = peek().byte_offset_start();

    bool isExported = false;
    if (peek().kind() == TokenKind::EXPORT) {
        expect(TokenKind::EXPORT);
        isExported = true;
    }

    expect(TokenKind::FN);

    ParsedFunctionSignature signature = parse_function_signature();

    expect(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::FunctionDefinition>(
        std::move(signature.identifier_), std::move(signature.parameters_), signature.returnType_,
        isExported, std::move(body), sourceStartIndex, body->source_end_index());
}

std::unique_ptr<AST::ConstantDefinition> Parser::parse_constant_definition() {
    const Token& constTok = expect(TokenKind::CONST);

    auto identifier = parse_identifier();

    Type type = PrimitiveType::ANY;
    if (peek().kind() == TokenKind::COLON) {
        expect(TokenKind::COLON);
        type = parse_type_specifier();
    }

    expect(TokenKind::EQUAL);
    auto value = parse_expression();
    const Token& semi = expect(TokenKind::SEMICOLON);

    return std::make_unique<AST::ConstantDefinition>(std::move(identifier), type, std::move(value),
                                                     constTok.byte_offset_start(),
                                                     semi.byte_offset_end());
}

std::unique_ptr<AST::Program> Parser::parse_program() {
    auto program = std::make_unique<AST::Program>();

    while (peek().kind() == TokenKind::CONST) {
        auto constant = parse_constant_definition();
        program->append_constant(std::move(constant));
    }
    while (peek().kind() == TokenKind::EXTERN) {
        auto externFunction = parse_external_function_declaration();
        program->append_extern_function(std::move(externFunction));
    }
    while (peek().kind() != TokenKind::EOF_) {
        if (peek().kind() == TokenKind::FN || peek().kind() == TokenKind::EXPORT) {
            auto functionDefinition = parse_function_definition();
            program->append_function(std::move(functionDefinition));
        } else {
            const std::string errorMessage =
                std::format("Invalid token -> expected function definition, got {}",
                            token_kind_to_string(peek().kind()));
            abort(errorMessage);
        }
    }

    return program;
}
