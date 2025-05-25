#include "parsing/parser.hpp"

#include <format>
#include <functional>
#include <iostream>
#include <memory>
#include <set>
#include <string>

#include "lexing/token.hpp"
#include "utils/log.hpp"

std::unique_ptr<AST::Program> Parser::parse() {
    auto program = parse_program();
    std::cout << "\033[1;32mParsing completed successfully.\033[0m\n";
    return program;
}

void Parser::abort(const std::string& errorMessage) {
    print_error(errorMessage);
    exit(EXIT_FAILURE);
}

const Token& Parser::peek(const int amount) const { return tokens_.at(currentIndex_ + amount); }

const Token& Parser::consume(const TokenKind expected) {
    const Token& token = peek();

    if (token.kind() != expected) {
        const std::string errorMessage =
            std::format("Invalid token at index {} -> expected {}, got {}", currentIndex_,
                        token_kind_to_string(expected), token_kind_to_string(token.kind()));
        abort(errorMessage);
    }

    currentIndex_++;
    return token;
}

Type Parser::parse_type_specifier() {
    const TokenKind tokenKind = peek().kind();
    consume(tokenKind);

    switch (tokenKind) {
        case TokenKind::INT:
            return RawType::INTEGER;
        case TokenKind::BOOL:
            return RawType::BOOLEAN;
        default: {
            const std::string errorMessage =
                std::format("Invalid token at index {} -> expected type specifier, got {}",
                            currentIndex_, token_kind_to_string(tokenKind));
            abort(errorMessage);
        }
    }
}

std::unique_ptr<AST::Identifier> Parser::parse_identifier() {
    const std::string& name = consume(TokenKind::IDENTIFIER).lexeme();
    return std::make_unique<AST::Identifier>(name);
}

std::unique_ptr<AST::FunctionCall> Parser::parse_function_call() {
    auto identifier = parse_identifier();

    consume(TokenKind::LEFT_PAREN);
    std::vector<std::unique_ptr<AST::Expression>> arguments;
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        arguments.push_back(parse_expression());
        if (peek().kind() == TokenKind::COMMA) {
            consume(TokenKind::COMMA);
        }
    }
    consume(TokenKind::RIGHT_PAREN);

    return std::make_unique<AST::FunctionCall>(std::move(identifier), std::move(arguments));
}

std::unique_ptr<AST::Expression> Parser::parse_primary_expression() {  // NOLINT(*-no-recursion)
    const Token& token = peek();

    switch (token.kind()) {
        case TokenKind::NUMBER_LITERAL:
            consume(TokenKind::NUMBER_LITERAL);
            return std::make_unique<AST::NumberLiteral>(std::stoll(token.lexeme()));

        case TokenKind::TRUE:
            consume(TokenKind::TRUE);
            return std::make_unique<AST::BooleanLiteral>(true);
        case TokenKind::FALSE:
            consume(TokenKind::FALSE);
            return std::make_unique<AST::BooleanLiteral>(false);

        case TokenKind::IDENTIFIER:
            if (peek(1).kind() == TokenKind::LEFT_PAREN) {
                return parse_function_call();
            } else {
                consume(TokenKind::IDENTIFIER);
                return std::make_unique<AST::Identifier>(token.lexeme());
            }

        case TokenKind::LEFT_PAREN: {
            consume(TokenKind::LEFT_PAREN);
            auto inner = parse_expression();
            consume(TokenKind::RIGHT_PAREN);
            return inner;
        }

        default:
            const std::string errorMessage = std::format(
                "Invalid token at beginning of primary expression at index {} -> got {}",
                currentIndex_, token_kind_to_string(token.kind()));
            abort(errorMessage);
    }
}

std::unique_ptr<AST::Expression> Parser::parse_unary_expression() {  // NOLINT(*-no-recursion)
    const Token& token = peek();

    const AST::Operator op = AST::token_kind_to_operator(token.kind());
    if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
        op == AST::Operator::LOGICAL_NOT) {
        consume(token.kind());
        auto operand = parse_primary_expression();
        return std::make_unique<AST::UnaryExpression>(op, std::move(operand));
    }

    return parse_primary_expression();
}

std::unique_ptr<AST::Expression> Parser::parse_binary_expression(
    const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
    const std::set<AST::Operator>& allowedOps, const bool allowMultiple) {
    auto left = parseOperand();
    while (true) {
        const Token& token = peek();
        const AST::Operator op = AST::token_kind_to_operator(token.kind());
        if (allowedOps.contains(op)) {
            consume(token.kind());
            auto right = parseOperand();
            left = std::make_unique<AST::BinaryExpression>(std::move(left), op, std::move(right));

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
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExpressionStatement>(std::move(expression));
}

std::unique_ptr<AST::VariableAssignment> Parser::parse_variable_assignment() {
    auto identifier = parse_identifier();
    consume(TokenKind::EQUAL);
    auto value = parse_expression();
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::VariableAssignment>(std::move(identifier), std::move(value));
}

std::unique_ptr<AST::VariableDeclaration> Parser::parse_variable_declaration() {
    consume(TokenKind::LET);

    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        consume(TokenKind::MUT);
        isMutable = true;
    }

    auto identifier = parse_identifier();

    Type type = RawType::ANY;
    if (peek().kind() == TokenKind::COLON) {
        consume(TokenKind::COLON);
        type = parse_type_specifier();
    }

    consume(TokenKind::EQUAL);
    auto value = parse_expression();
    consume(TokenKind::SEMICOLON);

    return std::make_unique<AST::VariableDeclaration>(std::move(identifier), type, isMutable,
                                                      std::move(value));
}

std::unique_ptr<AST::IfStatement> Parser::parse_if_statement() {  // NOLINT(*-no-recursion)
    consume(TokenKind::IF);
    auto condition = parse_expression();
    consume(TokenKind::COLON);

    auto body = parse_block_statement();

    auto ifStmt = std::make_unique<AST::IfStatement>(std::move(condition), std::move(body));

    AST::IfStatement* lastIf = ifStmt.get();

    while (peek().kind() == TokenKind::ELIF) {
        consume(TokenKind::ELIF);
        auto elifCondition = parse_expression();
        consume(TokenKind::COLON);
        auto elifBody = parse_block_statement();

        auto elifStmt = std::make_unique<AST::BlockStatement>();
        elifStmt->append_statement(
            std::make_unique<AST::IfStatement>(std::move(elifCondition), std::move(elifBody)));

        lastIf->elseClause_ = std::move(elifStmt);
        lastIf = static_cast<AST::IfStatement*>(lastIf->elseClause_->body_[0].get());
    }

    if (peek().kind() == TokenKind::ELSE) {
        consume(TokenKind::ELSE);
        consume(TokenKind::COLON);
        auto elseBody = parse_block_statement();
        lastIf->elseClause_ = std::move(elseBody);
    }

    return ifStmt;
}

std::unique_ptr<AST::WhileStatement> Parser::parse_while_statement() {  // NOLINT(*-no-recursion)
    consume(TokenKind::WHILE);
    auto condition = parse_expression();
    consume(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::WhileStatement>(std::move(condition), std::move(body));
}

std::unique_ptr<AST::VariableDeclaration> Parser::parse_function_parameter() {
    bool isMutable = false;
    if (peek().kind() == TokenKind::MUT) {
        consume(TokenKind::MUT);
        isMutable = true;
    }

    auto identifier = parse_identifier();

    consume(TokenKind::COLON);
    const Type type = parse_type_specifier();

    return std::make_unique<AST::VariableDeclaration>(std::move(identifier), type, isMutable);
}

std::unique_ptr<AST::BreakStatement> Parser::parse_break_statement() {
    consume(TokenKind::BREAK);
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::BreakStatement>();
}

std::unique_ptr<AST::ContinueStatement> Parser::parse_continue_statement() {
    consume(TokenKind::CONTINUE);
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::ContinueStatement>();
}

std::unique_ptr<AST::ReturnStatement> Parser::parse_return_statement() {
    consume(TokenKind::RETURN);
    auto returnValue = parse_expression();
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::ReturnStatement>(std::move(returnValue));
}

std::unique_ptr<AST::ExitStatement> Parser::parse_exit_statement() {
    consume(TokenKind::EXIT);
    auto exitCode = parse_expression();
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::ExitStatement>(std::move(exitCode));
}

std::unique_ptr<AST::BlockStatement> Parser::parse_block_statement() {  // NOLINT(*-no-recursion)
    consume(TokenKind::LEFT_BRACE);
    auto block = std::make_unique<AST::BlockStatement>();
    while (peek().kind() != TokenKind::RIGHT_BRACE) {
        block->append_statement(parse_statement());
    }
    consume(TokenKind::RIGHT_BRACE);
    return block;
}

std::unique_ptr<AST::Statement> Parser::parse_statement() {  // NOLINT(*-no-recursion)
    const TokenKind tokenKind = peek().kind();

    if (tokenKind == TokenKind::LET) return parse_variable_declaration();
    if (tokenKind == TokenKind::IDENTIFIER && peek(1).kind() == TokenKind::EQUAL)
        return parse_variable_assignment();
    if (tokenKind == TokenKind::IF) return parse_if_statement();
    if (tokenKind == TokenKind::WHILE) return parse_while_statement();
    if (tokenKind == TokenKind::BREAK) return parse_break_statement();
    if (tokenKind == TokenKind::CONTINUE) return parse_continue_statement();
    if (tokenKind == TokenKind::RETURN) return parse_return_statement();
    if (tokenKind == TokenKind::EXIT) return parse_exit_statement();
    if (tokenKind == TokenKind::LEFT_BRACE) return parse_block_statement();
    return parse_expression_statement();
}

std::unique_ptr<AST::FunctionDeclaration>
Parser::parse_function_declaration() {  // NOLINT(*-no-recursion)
    consume(TokenKind::FN);
    auto identifier = parse_identifier();

    consume(TokenKind::LEFT_PAREN);
    std::vector<std::unique_ptr<AST::VariableDeclaration>> parameters;
    while (peek().kind() != TokenKind::RIGHT_PAREN) {
        parameters.push_back(parse_function_parameter());
        if (peek().kind() == TokenKind::COMMA) {
            consume(TokenKind::COMMA);
        }
    }
    consume(TokenKind::RIGHT_PAREN);

    Type returnType = RawType::VOID;
    if (peek().kind() == TokenKind::RIGHT_ARROW) {
        consume(TokenKind::RIGHT_ARROW);
        returnType = parse_type_specifier();
    }

    consume(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::FunctionDeclaration>(std::move(identifier), std::move(parameters),
                                                      returnType, std::move(body));
}

std::unique_ptr<AST::ConstantDeclaration> Parser::parse_constant_declaration() {
    consume(TokenKind::CONST);

    auto identifier = parse_identifier();

    Type type = RawType::ANY;
    if (peek().kind() == TokenKind::COLON) {
        consume(TokenKind::COLON);
        type = parse_type_specifier();
    }

    consume(TokenKind::EQUAL);
    auto value = parse_expression();
    consume(TokenKind::SEMICOLON);

    return std::make_unique<AST::ConstantDeclaration>(std::move(identifier), type,
                                                      std::move(value));
}

std::unique_ptr<AST::Program> Parser::parse_program() {
    auto program = std::make_unique<AST::Program>();

    while (peek().kind() != TokenKind::FN) {
        if (peek().kind() == TokenKind::CONST) {
            auto constant = parse_constant_declaration();
            program->append_constant(std::move(constant));
        } else {
            const std::string errorMessage =
                std::format("Invalid token at index {} -> expected constant declaration, got {}",
                            currentIndex_, token_kind_to_string(peek().kind()));
            abort(errorMessage);
        }
    }
    while (peek().kind() != TokenKind::EOF_) {
        if (peek().kind() == TokenKind::FN) {
            auto functionDeclaration = parse_function_declaration();
            program->append_function(std::move(functionDeclaration));
        } else {
            const std::string errorMessage =
                std::format("Invalid token at index {} -> expected function declaration, got {}",
                            currentIndex_, token_kind_to_string(peek().kind()));
            abort(errorMessage);
        }
    }

    return program;
}
