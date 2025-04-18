#pragma once

#include <format>
#include <string>
#include <vector>

#include "token.hpp"
#include "token_kind.hpp"
#include "utils/log.hpp"

class Lexer {
   private:
    size_t currentIndex_ = 0;
    const std::string source_;
    std::string buffer_;

    std::vector<Token> tokens_;

    [[nodiscard]] bool is_at_end() const { return currentIndex_ >= source_.length(); }
    [[nodiscard]] char peek() const { return is_at_end() ? '\0' : source_.at(currentIndex_); }
    char advance() { return source_.at(currentIndex_++); }

    void read_to_buffer_while(auto predicate) {
        while (!is_at_end() && predicate(peek())) {
            buffer_ += advance();
        }
    }

    std::optional<TokenKind> get_keyword_kind() {
        if (buffer_ == "true") return TokenKind::TRUE;
        if (buffer_ == "false") return TokenKind::FALSE;
        if (buffer_ == "let") return TokenKind::LET;
        if (buffer_ == "if") return TokenKind::IF;
        if (buffer_ == "exit") return TokenKind::EXIT;

        return std::nullopt;
    }

    void lex_equal() {
        if (peek() == '=') {
            advance();
            tokens_.emplace_back(TokenKind::EQUAL_EQUAL, "==");
        } else {
            tokens_.emplace_back(TokenKind::EQUAL, "=");
        }
    }

    void lex_less_than() {
        if (peek() == '=') {
            advance();
            tokens_.emplace_back(TokenKind::LESS_THAN_EQUAL, "<=");
        } else {
            tokens_.emplace_back(TokenKind::LESS_THAN, "<");
        }
    }

    void lex_greater_than() {
        if (peek() == '=') {
            advance();
            tokens_.emplace_back(TokenKind::GREATER_THAN_EQUAL, ">=");
        } else {
            tokens_.emplace_back(TokenKind::GREATER_THAN, ">");
        }
    }

    void lex_bang() {
        if (peek() == '=') {
            advance();
            tokens_.emplace_back(TokenKind::NOT_EQUAL, "!=");
        } else {
            tokens_.emplace_back(TokenKind::BANG, "!");
        }
    }

   public:
    explicit Lexer(std::string source) : source_(std::move(source)) {}

    [[nodiscard]] std::vector<Token> tokenize() {
        while (!is_at_end()) {
            char c = advance();
            buffer_ = c;

            if (std::isspace(c)) continue;

            if (c == '#') {
                while (!is_at_end() && peek() != '\n') advance();
                continue;
            }

            if (std::isalpha(c)) {
                read_to_buffer_while(isalnum);
                if (auto keywordKind = get_keyword_kind()) {
                    tokens_.emplace_back(*keywordKind, buffer_);
                } else {
                    tokens_.emplace_back(TokenKind::IDENTIFIER, buffer_);
                }

            } else if (std::isdigit(c)) {
                read_to_buffer_while(isdigit);
                tokens_.emplace_back(TokenKind::NUMBER_LITERAL, buffer_);
            } else if (c == '+') {
                tokens_.emplace_back(TokenKind::PLUS, "+");
            } else if (c == '-') {
                tokens_.emplace_back(TokenKind::MINUS, "-");
            } else if (c == '*') {
                tokens_.emplace_back(TokenKind::STAR, "*");
            } else if (c == '/') {
                tokens_.emplace_back(TokenKind::SLASH, "/");
            } else if (c == '!') {
                lex_bang();
            } else if (c == '=') {
                lex_equal();
            } else if (c == '<') {
                lex_less_than();
            } else if (c == '>') {
                lex_greater_than();
            } else if (c == '(') {
                tokens_.emplace_back(TokenKind::LEFT_PAREN, "(");
            } else if (c == ')') {
                tokens_.emplace_back(TokenKind::RIGHT_PAREN, ")");
            } else if (c == '{') {
                tokens_.emplace_back(TokenKind::LEFT_BRACE, "{");
            } else if (c == '}') {
                tokens_.emplace_back(TokenKind::RIGHT_BRACE, "}");
            } else if (c == ':') {
                tokens_.emplace_back(TokenKind::COLON, ":");
            } else if (c == ';') {
                tokens_.emplace_back(TokenKind::SEMICOLON, ";");
            } else {
                const std::string errorMessage =
                    std::format("Invalid character at index {}, got '{}' at beginning of word",
                                currentIndex_ - 1, c);
                print_error(errorMessage);
                exit(EXIT_FAILURE);
            }
        }
        tokens_.emplace_back(TokenKind::END_OF_FILE, "");
        return tokens_;
    }
};
