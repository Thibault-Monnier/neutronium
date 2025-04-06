#pragma once

#include <cstdlib>
#include <format>
#include <memory>
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

    const Token& peek() const { return tokens_.at(currentIndex_); }

    const Token& consume(const TokenKind expected) {
        const Token& token = peek();

        if (token.kind() != expected) {
            const std::string errorMessage =
                std::format("Invalid token at index {} -> expected {}, got {}", currentIndex_,
                            token_kind_to_string(expected), token_kind_to_string(token.kind()));
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        currentIndex_++;
        return token;
    }

    const bool statement_end() const {
        return peek().kind() == TokenKind::NEWLINE || peek().kind() == TokenKind::END_OF_FILE;
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
            switch (token.kind()) {
                case TokenKind::NUMBER_LITERAL:
                    return AST::NumberLiteral(std::stoi(token.lexeme()));
                case TokenKind::IDENTIFIER:
                    return AST::Identifier(token.lexeme());
                default:
                    const std::string errorMessage =
                        std::format("Invalid token in primary expression at index {} -> got {}",
                                    currentIndex_, token_kind_to_string(token.kind()));
                    print_error(errorMessage);
                    exit(EXIT_FAILURE);
            }
        }

        if (tokens_.at(startIndex).kind() == TokenKind::LEFT_PAREN &&
            tokens_.at(endIndex - 1).kind() == TokenKind::RIGHT_PAREN) {
            int parenCount = 1;
            size_t i = startIndex + 1;
            while (parenCount > 0 && i < endIndex) {
                if (tokens_.at(i).kind() == TokenKind::LEFT_PAREN) parenCount++;
                if (tokens_.at(i).kind() == TokenKind::RIGHT_PAREN) parenCount--;
                i++;
            }

            if (i == endIndex) {
                return parse_expression_recursive(startIndex + 1, endIndex - 1);
            }
        }

        constexpr std::array operatorsByPrecedenceLevel = {
            AST::Operator::ADD, AST::Operator::SUBTRACT, AST::Operator::MULTIPLY,
            AST::Operator::DIVIDE};

        for (auto op : operatorsByPrecedenceLevel) {
            for (size_t i = endIndex - 1; i >= startIndex; i--) {
                const Token token = tokens_.at(i);

                if (token.kind() == TokenKind::RIGHT_PAREN) {
                    size_t parenCount = 1;
                    while (parenCount > 0 && i < endIndex) {
                        i--;
                        if (tokens_.at(i).kind() == TokenKind::LEFT_PAREN) parenCount--;
                        if (tokens_.at(i).kind() == TokenKind::RIGHT_PAREN) parenCount++;
                    }

                    if (parenCount != 0) {
                        const std::string errorMessage =
                            std::format("Unmatched parentheses at index {}", i);
                        const std::string hintMessage =
                            std::format("Start index: {}, End index: {}", startIndex, endIndex);
                        print_error(errorMessage);
                        print_hint(hintMessage);
                        exit(EXIT_FAILURE);
                    }

                    continue;
                }

                if (token.kind() == TokenKind::LEFT_PAREN) {
                    const std::string errorMessage =
                        std::format("Unmatched parentheses at index {}", i);
                    const std::string hintMessage =
                        std::format("Start index: {}, End index: {}", startIndex, endIndex);
                    print_error(errorMessage);
                    print_hint(hintMessage);
                    exit(EXIT_FAILURE);
                }

                if (AST::token_kind_to_AST_operator(token.kind()) == op) {
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
            consume(peek().kind());
        }

        return parse_expression_recursive(startIndex, currentIndex_);
    }

    AST::Assignment parse_assignment() {
        consume(TokenKind::LET);
        const std::string identifier = consume(TokenKind::IDENTIFIER).lexeme();
        consume(TokenKind::EQUAL);
        const AST::Expression value = parse_expression();

        return {identifier, value};
    }

    AST::Statement parse_statement() {
        const Token& firstToken = peek();

        if (firstToken.kind() == TokenKind::LET) {
            return parse_assignment();
        }

        const std::string errorMessage =
            std::format("Invalid token at index {} -> got {} at beginning of statement",
                        currentIndex_, token_kind_to_string(firstToken.kind()));
        const std::string hintMessage = std::format("Lexeme -> {}", firstToken.lexeme());
        print_error(errorMessage);
        print_hint(hintMessage);

        exit(EXIT_FAILURE);
    }

   public:
    explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    AST::Program parse() {
        auto program = AST::Program();
        while (peek().kind() != TokenKind::END_OF_FILE) {
            while (peek().kind() == TokenKind::NEWLINE) consume(TokenKind::NEWLINE);
            if (peek().kind() == TokenKind::END_OF_FILE) break;
            program.statements_.emplace_back(parse_statement());
        }

        const std::shared_ptr<AST::ASTNode> programPtr =
            std::make_shared<AST::Program>(std::move(program));
        AST::log_node(programPtr);

        return program;
    }
};
