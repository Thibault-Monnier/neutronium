#pragma once

#include <string>

#include "source_location.hpp"
#include "token_kind.hpp"

class Token {
   public:
    explicit Token(const TokenKind kind, std::string lexeme, SourceLocation location)
        : kind_(kind), lexeme_(std::move(lexeme)), location_(std::move(location)) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] const std::string& lexeme() const { return lexeme_; }

    [[nodiscard]] const SourceLocation& location() const { return location_; }

   private:
    TokenKind kind_;
    std::string lexeme_;

    SourceLocation location_;
};
