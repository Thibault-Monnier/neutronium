#pragma once

#include <string>

#include "TokenKind.hpp"

class Token {
   public:
    explicit Token(const TokenKind kind, std::string_view lexeme, const uint32_t byteOffset)
        : kind_(kind), lexeme_(lexeme), byteOffset_(byteOffset) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] std::string_view lexeme() const { return lexeme_; }

    [[nodiscard]] uint32_t byteOffsetStart() const { return byteOffset_; }
    [[nodiscard]] uint32_t byteOffsetEnd() const {
        return byteOffset_ + static_cast<uint32_t>(lexeme_.size()) - 1;
    }

   private:
    TokenKind kind_;
    std::string_view lexeme_;

    const uint32_t byteOffset_;
};
