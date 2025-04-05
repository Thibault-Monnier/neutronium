#pragma once

#include <string>

#include "token_type.hpp"

struct Token {
    explicit Token(const TokenType type, std::string lexeme)
        : type_(type), lexeme_(std::move(lexeme)) {}

    [[nodiscard]] TokenType type() const { return type_; }
    [[nodiscard]] const std::string &lexeme() const { return lexeme_; }

   private:
    TokenType type_;
    std::string lexeme_;
};
