#pragma once

#include <string>

#include "source/source_manager.hpp"
#include "token_kind.hpp"

class Token {
   public:
    explicit Token(const TokenKind kind, std::string lexeme, const int byteOffset)
        : kind_(kind), lexeme_(std::move(lexeme)), byteOffset_(byteOffset) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] const std::string& lexeme() const { return lexeme_; }

    [[nodiscard]] int byte_offset() const { return byteOffset_; }

   private:
    TokenKind kind_;
    std::string lexeme_;

    const int byteOffset_;
};
