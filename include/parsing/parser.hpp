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
    const Token& peek(size_t amount) const {
        const size_t index = currentIndex_ + amount;
        if (index >= tokens_.size()) {
            throw std::out_of_range("Peek index out of range");
        }

        return tokens_.at(index);
    }

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
        if (endIndex - startIndex == 1) {
            const Token token = tokens_[startIndex];
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

        for (AST::Operator const op : operatorsByPrecedenceLevel) {
            for (size_t i = startIndex; i < endIndex - 1; i+=2) {
                const Token primaryToken = tokens_[i];
                const Token opToken = tokens_[i + 1];

                if (primaryToken.type() != TokenType::NUMBER) {
                    const std::string errorMessage =
                        std::format("Invalid token in primary expression at index {} -> got {}",
                                    i, token_type_to_string(primaryToken.type()));
                    const std::string hintMessage =
                        std::format("Lexeme -> {}", primaryToken.lexeme());
                    print_error(errorMessage);
                    print_hint(hintMessage);
                    exit(EXIT_FAILURE);
                }

                if (token_type_to_AST_operator(opToken.type()) == op) {
                    return std::make_shared<AST::BinaryExpression>(
                        parse_expression_recursive(startIndex, i+1), op,
                        parse_expression_recursive(i + 2, endIndex));
                }
            }
        }

        const std::string errorMessage =
            std::format("No binary operator found in binary expression at index {}", startIndex);
        print_error(errorMessage);
        const std::string hintMessage =
            std::format("Start index: {}, End index: {}", startIndex, endIndex);
        print_hint(hintMessage);
        exit(EXIT_FAILURE);
    }

    AST::Expression parse_expression() {
        size_t endIndex = currentIndex_;

        size_t i = 0;
        while (i < tokens_.size() && peek(i).type() != TokenType::NEWLINE &&
               peek(i).type() != TokenType::END_OF_FILE) {
            endIndex++, i++;
        }

        AST::Expression expression = parse_expression_recursive(currentIndex_, endIndex);

        currentIndex_ = endIndex;

        return expression;
    }

    AST::Assignment parse_assignment() {
        const std::string identifier = consume(TokenType::IDENTIFIER).lexeme();
        consume(TokenType::EQUAL);
        const AST::Expression value = parse_expression();

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
            if (peek().type() == TokenType::END_OF_FILE) break;
            program.statements_.emplace_back(parse_statement());
        }

        const std::shared_ptr<AST::ASTNode> programPtr =
            std::make_shared<AST::Program>(std::move(program));

        AST::log_node(programPtr);

        return program;
    }
};
