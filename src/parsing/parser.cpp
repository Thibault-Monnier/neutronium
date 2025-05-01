#include "parsing/parser.hpp"

#include <format>
#include <functional>
#include <iostream>
#include <memory>
#include <set>
#include <string>

#include "lexing/token.hpp"
#include "parsing/AST.hpp"
#include "utils/log.hpp"

AST::Program Parser::parse() {
    auto program = AST::Program();
    while (peek().kind() != TokenKind::EOF_) {
        program.body_->append_statement(parse_statement());
    }

    AST::log_ast(program);

    return program;
}

void Parser::abort(const std::string& errorMessage, const std::string& hintMessage) {
    print_error(errorMessage);
    if (!hintMessage.empty()) {
        print_hint(hintMessage);
    }
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
            consume(TokenKind::IDENTIFIER);
            return std::make_unique<AST::Identifier>(token.lexeme());

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

std::unique_ptr<AST::Expression> Parser::parse_relational_expression() {
    return parse_binary_expression(
        [this]() { return parse_additive_expression(); },
        std::set{AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
                 AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
                 AST::Operator::GREATER_THAN_OR_EQUAL},
        false);
}

std::unique_ptr<AST::Expression> Parser::parse_expression() {
    return parse_relational_expression();
}

std::unique_ptr<AST::Identifier> Parser::parse_identifier() {
    const std::string name = consume(TokenKind::IDENTIFIER).lexeme();
    return std::make_unique<AST::Identifier>(name);
}

std::unique_ptr<AST::Assignment> Parser::parse_assignment(const bool isDeclaration) {
    if (isDeclaration) {
        consume(TokenKind::LET);
    }
    auto identifier = parse_identifier();
    consume(TokenKind::EQUAL);
    auto value = parse_expression();
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::Assignment>(std::move(identifier), std::move(value),
                                             isDeclaration);
}

std::unique_ptr<AST::IfStatement> Parser::parse_if_statement() {  // NOLINT(*-no-recursion)
    consume(TokenKind::IF);
    auto condition = parse_expression();
    consume(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::IfStatement>(std::move(condition), std::move(body));
}

std::unique_ptr<AST::WhileStatement> Parser::parse_while_statement() {  // NOLINT(*-no-recursion)
    consume(TokenKind::WHILE);
    auto condition = parse_expression();
    consume(TokenKind::COLON);
    auto body = parse_block_statement();
    std::cout << "Parsed while statement with condition: " << '\n';
    return std::make_unique<AST::WhileStatement>(std::move(condition), std::move(body));
}

std::unique_ptr<AST::FunctionDeclaration> Parser::parse_function_declaration() {
    consume(TokenKind::FN);
    auto identifier = parse_identifier();
    consume(TokenKind::COLON);
    auto body = parse_block_statement();
    return std::make_unique<AST::FunctionDeclaration>(
        std::move(identifier), std::vector<std::unique_ptr<AST::Identifier>>{}, std::move(body));
}

std::unique_ptr<AST::Exit> Parser::parse_exit() {
    consume(TokenKind::EXIT);
    auto exitCode = parse_expression();
    consume(TokenKind::SEMICOLON);
    return std::make_unique<AST::Exit>(std::move(exitCode));
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
    const Token& firstToken = peek();

    switch (firstToken.kind()) {
        case TokenKind::LET:
            return parse_assignment(true);
        case TokenKind::IDENTIFIER:
            return parse_assignment(false);
        case TokenKind::IF:
            return parse_if_statement();
        case TokenKind::WHILE:
            return parse_while_statement();
        case TokenKind::FN:
            return parse_function_declaration();
        case TokenKind::EXIT:
            return parse_exit();
        case TokenKind::LEFT_BRACE:
            return parse_block_statement();
        default:
            const std::string errorMessage =
                std::format("Invalid token at index {} -> got {} at beginning of statement",
                            currentIndex_, token_kind_to_string(firstToken.kind()));
            const std::string hintMessage = std::format("Lexeme -> {}", firstToken.lexeme());
            abort(errorMessage, hintMessage);
    }
}