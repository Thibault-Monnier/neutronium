#pragma once

#include "TokenKind.hpp"

class alignas(4) Token {
   public:
    Token(const TokenKind kind, const uint32_t startByteOffset, const uint16_t length)
        : kind_(kind), length_(length), startByteOffset_(startByteOffset) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }

    [[nodiscard]] uint32_t byteOffsetStart() const { return startByteOffset_; }
    [[nodiscard]] uint32_t byteOffsetEnd() const { return startByteOffset_ + length_ - 1; }

    [[nodiscard]] std::string_view lexeme(const std::string_view source) const {
        return {source.data() + startByteOffset_, length_};
    }

   private:
    TokenKind kind_;

    const uint16_t length_;

    const uint32_t startByteOffset_;
};
