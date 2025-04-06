#pragma once

#include <string>

#include "token_kind.hpp"

struct Token {
    explicit Token(const TokenKind kind, std::string lexeme)
        : kind_(kind), lexeme_(std::move(lexeme)) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] const std::string &lexeme() const { return lexeme_; }

   private:
    TokenKind kind_;
    std::string lexeme_;
};
