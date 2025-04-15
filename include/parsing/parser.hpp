#pragma once

#include <cstdlib>
#include <format>
#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "lexing/token.hpp"
#include "lexing/token_kind.hpp"
#include "parsing/AST.hpp"
#include "utils/log.hpp"

class Parser {
   private:
    std::vector<Token> tokens_;
    size_t currentIndex_ = 0;

    [[noreturn]] void abort(const std::string& errorMessage, const std::string& hintMessage = "") {
        print_error(errorMessage);
        if (!hintMessage.empty()) {
            print_hint(hintMessage);
        }
        exit(EXIT_FAILURE);
    }

    const Token& peek() const { return tokens_.at(currentIndex_); }

    const Token& consume(const TokenKind expected) {
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

    std::unique_ptr<AST::Expression> parse_primary_expression() {  // NOLINT(*-no-recursion)
        const Token& token = peek();

        switch (token.kind()) {
            case TokenKind::NUMBER_LITERAL:
                consume(TokenKind::NUMBER_LITERAL);
                return std::make_unique<AST::NumberLiteral>(std::stoi(token.lexeme()));

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

    std::unique_ptr<AST::Expression> parse_unary_expression() {  // NOLINT(*-no-recursion)
        const Token& token = peek();

        const AST::Operator op = AST::token_kind_to_operator(token.kind());
        if (op == AST::Operator::ADD || op == AST::Operator::SUBTRACT) {
            consume(token.kind());
            auto operand = parse_primary_expression();
            return std::make_unique<AST::UnaryExpression>(op, std::move(operand));
        }

        return parse_primary_expression();
    }

    std::unique_ptr<AST::Expression> parse_binary_expression(
        const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
        const std::set<AST::Operator>& allowedOps, const bool allowMultiple) {
        auto left = parseOperand();
        while (true) {
            const Token& token = peek();
            const AST::Operator op = AST::token_kind_to_operator(token.kind());
            if (allowedOps.contains(op)) {
                consume(token.kind());
                auto right = parseOperand();
                left =
                    std::make_unique<AST::BinaryExpression>(std::move(left), op, std::move(right));

                if (!allowMultiple) {
                    break;
                }
            } else {
                break;
            }
        }

        return left;
    }

    std::unique_ptr<AST::Expression> parse_multiplicative_expression() {
        return parse_binary_expression([this]() { return parse_unary_expression(); },
                                       std::set{AST::Operator::MULTIPLY, AST::Operator::DIVIDE},
                                       true);
    }

    std::unique_ptr<AST::Expression> parse_additive_expression() {
        return parse_binary_expression([this]() { return parse_multiplicative_expression(); },
                                       std::set{AST::Operator::ADD, AST::Operator::SUBTRACT}, true);
    }

    std::unique_ptr<AST::Expression> parse_relational_expression() {
        return parse_binary_expression(
            [this]() { return parse_additive_expression(); },
            std::set{AST::Operator::EQUALS, AST::Operator::NOT_EQUALS, AST::Operator::LESS_THAN,
                     AST::Operator::GREATER_THAN, AST::Operator::LESS_THAN_OR_EQUAL,
                     AST::Operator::GREATER_THAN_OR_EQUAL},
            false);
    }

    std::unique_ptr<AST::Expression> parse_expression() { return parse_relational_expression(); }

    std::unique_ptr<AST::Assignment> parse_assignment(const bool isDeclaration) {
        if (isDeclaration) {
            consume(TokenKind::LET);
        }
        const std::string identifierName = consume(TokenKind::IDENTIFIER).lexeme();
        auto identifier = std::make_unique<AST::Identifier>(identifierName);
        consume(TokenKind::EQUAL);
        auto value = parse_expression();
        consume(TokenKind::SEMICOLON);
        return std::make_unique<AST::Assignment>(std::move(identifier), std::move(value), isDeclaration);
    }

    std::unique_ptr<AST::Exit> parse_exit() {
        consume(TokenKind::EXIT);
        auto exitCode = parse_expression();
        consume(TokenKind::SEMICOLON);
        return std::make_unique<AST::Exit>(std::move(exitCode));
    }

    std::unique_ptr<AST::Statement> parse_statement() {
        const Token& firstToken = peek();

        switch (firstToken.kind()) {
            case TokenKind::LET:
                return parse_assignment(true);
            case TokenKind::IDENTIFIER:
                return parse_assignment(false);
            case TokenKind::EXIT:
                return parse_exit();
            default:
                const std::string errorMessage =
                    std::format("Invalid token at index {} -> got {} at beginning of statement",
                                currentIndex_, token_kind_to_string(firstToken.kind()));
                const std::string hintMessage = std::format("Lexeme -> {}", firstToken.lexeme());
                abort(errorMessage, hintMessage);
        }
    }

   public:
    explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    AST::Program parse() {
        auto program = AST::Program();
        while (peek().kind() != TokenKind::END_OF_FILE) {
            program.append_statement(parse_statement());
        }

        AST::log_ast(program);

        return program;
    }
};
