#pragma once

#include <format>
#include <string>
#include <vector>

#include "token.hpp"
#include "token_kind.hpp"
#include "utils/log.hpp"

class Lexer {
   public:
    explicit Lexer(std::string source) : source_(std::move(source)) {}

    [[nodiscard]] std::vector<Token> tokenize() {
        std::vector<Token> tokens;

        while (!is_at_end()) {
            char c = advance();
            buffer_ = c;

            if (std::isspace(c)) {
                if (c == '\n') tokens.emplace_back(TokenKind::NEWLINE, "\n");

            } else if (std::isalpha(c)) {
                read_to_buffer_while(isalnum);
                tokens.emplace_back(TokenKind::IDENTIFIER, buffer_);
            } else if (std::isdigit(c)) {
                read_to_buffer_while(isdigit);
                tokens.emplace_back(TokenKind::NUMBER_LITERAL, buffer_);
            } else if (c == '+') {
                tokens.emplace_back(TokenKind::PLUS, "+");
            } else if (c == '-') {
                tokens.emplace_back(TokenKind::MINUS, "-");
            } else if (c == '*') {
                tokens.emplace_back(TokenKind::STAR, "*");
            } else if (c == '/') {
                tokens.emplace_back(TokenKind::SLASH, "/");
            } else if (c == '=') {
                tokens.emplace_back(TokenKind::EQUAL, "=");
            } else if (c == '(') {
                tokens.emplace_back(TokenKind::LEFT_PAREN, "(");
            } else if (c == ')') {
                tokens.emplace_back(TokenKind::RIGHT_PAREN, ")");
            } else {
                const std::string errorMessage =
                    std::format("Invalid character at index {}, got '{}' at beginning of word",
                                currentIndex_ - 1, c);
                print_error(errorMessage);
                exit(EXIT_FAILURE);
            }
        }

        tokens.emplace_back(TokenKind::END_OF_FILE, "");
        return tokens;
    }

   private:
    size_t currentIndex_ = 0;
    const std::string source_;
    std::string buffer_;

    [[nodiscard]] bool is_at_end() const { return currentIndex_ >= source_.length(); }
    [[nodiscard]] char peek() const { return is_at_end() ? '\0' : source_.at(currentIndex_); }
    char advance() { return source_.at(currentIndex_++); }

    void read_to_buffer_while(auto predicate) {
        while (!is_at_end() && predicate(peek())) {
            buffer_ += advance();
        }
    }
};
