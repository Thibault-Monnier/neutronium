#pragma once

#include "TokenKind.hpp"

class Token {
   public:
    explicit Token(const TokenKind kind, const std::string_view lexeme, const uint32_t byteOffset)
        : lexeme_(lexeme), kind_(kind), byteOffset_(byteOffset) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] std::string_view lexeme() const { return lexeme_; }

    [[nodiscard]] uint32_t byteOffsetStart() const { return byteOffset_; }
    [[nodiscard]] uint32_t byteOffsetEnd() const {
        return byteOffset_ + static_cast<uint32_t>(lexeme_.size()) - 1;
    }

   private:
    std::string_view lexeme_;

    TokenKind kind_;

    const uint32_t byteOffset_;
};
