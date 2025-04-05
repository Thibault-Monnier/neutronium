#pragma once

#include <cstdlib>
#include <format>
#include <memory>
#include <string>
#include <vector>

#include "lexing/token.hpp"
#include "lexing/token_type.hpp"
#include "parsing/AST_nodes.hpp"
#include "utils/log.hpp"

class Parser {
   private:
    std::vector<Token> tokens_;
    size_t currentIndex_ = 0;

    const Token& peek() const { return tokens_.at(currentIndex_); }

    const Token& consume(const TokenType expected) {
        const Token& token = peek();

        if (token.type() != expected) {
            const std::string errorMessage =
                std::format("Invalid token at index {} -> expected {}, got {}", currentIndex_,
                            token_type_to_string(expected), token_type_to_string(token.type()));
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        currentIndex_++;
        return token;
    }

    const bool statement_end() const {
        return peek().type() == TokenType::NEWLINE || peek().type() == TokenType::END_OF_FILE;
    }

    const AST::Operator token_type_to_AST_operator(const TokenType tokenType) const {
        switch (tokenType) {
            case TokenType::PLUS:
                return AST::Operator::ADD;
            case TokenType::MINUS:
                return AST::Operator::SUBTRACT;
            default:
                return AST::Operator::UNDEFINED_OPERATOR;
        }
    }

    AST::Expression parse_expression_recursive(size_t startIndex, size_t endIndex) {
        if (startIndex >= endIndex) {
            const std::string errorMessage = std::format(
                "Invalid expression at index {} -> start index is greater than end index",
                currentIndex_);
            const std::string hintMessage =
                std::format("Start index: {}, End index: {}", startIndex, endIndex);
            print_error(errorMessage);
            print_hint(hintMessage);
            exit(EXIT_FAILURE);
        }

        if (endIndex - startIndex == 1) {
            const Token token = tokens_.at(startIndex);
            switch (token.type()) {
                case TokenType::NUMBER:
                    return std::make_shared<AST::PrimaryExpression>(std::stoi(token.lexeme()));
                default:
                    const std::string errorMessage =
                        std::format("Invalid token in primary expression at index {} -> got {}",
                                    currentIndex_, token_type_to_string(token.type()));
                    print_error(errorMessage);
                    print_hint("Identifiers in expressions are not yet supported");
                    exit(EXIT_FAILURE);
            }
        }

        constexpr std::array operatorsByPrecedenceLevel = {AST::Operator::ADD,
                                                           AST::Operator::SUBTRACT};

        for (auto op : operatorsByPrecedenceLevel) {
            for (size_t i = startIndex; i < endIndex; i++) {
                const Token token = tokens_.at(i);

                if (token_type_to_AST_operator(token.type()) == op) {
                    return std::make_shared<AST::BinaryExpression>(
                        parse_expression_recursive(startIndex, i), op,
                        parse_expression_recursive(i + 1, endIndex));
                }
            }
        }

        const std::string errorMessage =
            std::format("Missing operator in binary expression at index {}", startIndex);
        const std::string hintMessage =
            std::format("Start index: {}, End index: {}", startIndex, endIndex);
        print_error(errorMessage);
        print_hint(hintMessage);
        exit(EXIT_FAILURE);
    }

    AST::Expression parse_expression() {
        size_t const startIndex = currentIndex_;

        while (!statement_end()) {
            consume(peek().type());
        }

        return parse_expression_recursive(startIndex, currentIndex_);
    }

    AST::Assignment parse_assignment() {
        const std::string identifier = consume(TokenType::IDENTIFIER).lexeme();
        consume(TokenType::EQUAL);
        const AST::Expression value = parse_expression();

        return {identifier, value};
    }

    AST::Statement parse_statement() {
        const Token& firstToken = peek();

        if (firstToken.type() == TokenType::IDENTIFIER) {
            return parse_assignment();
        }

        const std::string errorMessage =
            std::format("Invalid token at index {} -> got {} at beginning of statement",
                        currentIndex_, token_type_to_string(firstToken.type()));
        const std::string hintMessage = std::format("Lexeme -> {}", firstToken.lexeme());
        print_error(errorMessage);
        print_hint(hintMessage);

        exit(EXIT_FAILURE);
    }

   public:
    explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    AST::Program parse() {
        auto program = AST::Program();
        while (peek().type() != TokenType::END_OF_FILE) {
            while (peek().type() == TokenType::NEWLINE) consume(TokenType::NEWLINE);
            if (peek().type() == TokenType::END_OF_FILE) break;
            program.statements_.emplace_back(parse_statement());
        }

        const std::shared_ptr<AST::ASTNode> programPtr =
            std::make_shared<AST::Program>(std::move(program));
        AST::log_node(programPtr);

        return program;
    }
};
