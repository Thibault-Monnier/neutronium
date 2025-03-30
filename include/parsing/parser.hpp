#pragma once

#include <cstdlib>
#include <format>
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
                std::format("Unexpected token at index {} -> expected {}, got {}", currentIndex_,
                            token_type_to_string(expected), token_type_to_string(token.type()));
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        currentIndex_++;
        return token;
    }

    AST::Expression parse_expression() {
        std::string value = "";

        auto next = TokenType::END_OF_FILE;
        while ((next = peek().type()) != TokenType::END_OF_FILE && next != TokenType::NEWLINE) {
            value += consume(next).lexeme();
            value += ' ';
        }

        return {value};
    }

    AST::Assignment parse_assignment() {
        const std::string identifier = consume(TokenType::IDENTIFIER).lexeme();
        consume(TokenType::EQUAL);
        const std::string value = parse_expression().value_;

        return {identifier, value};
    }

    AST::Statement parse_statement() {
        const Token& token = peek();

        if (token.type() == TokenType::IDENTIFIER) {
            return parse_assignment();
        }

        const std::string errorMessage =
            std::format("Invalid token at index {} -> got {} at beginning of statement",
                        currentIndex_, token_type_to_string(token.type()));
        print_error(errorMessage);

        const std::string hintMessage = std::format("Lexeme -> {}", token.lexeme());
        print_hint(hintMessage);

        exit(EXIT_FAILURE);
    }

   public:
    explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    AST::Program parse() {
        auto program = AST::Program();
        while (peek().type() != TokenType::END_OF_FILE) {
            while (peek().type() == TokenType::NEWLINE) consume(TokenType::NEWLINE);
            program.statements_.emplace_back(parse_statement());
        }

        return program;
    }
};
