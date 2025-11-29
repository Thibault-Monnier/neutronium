#pragma once

#include "TokenKind.hpp"

class alignas(4) Token {
   public:
    explicit Token(const TokenKind kind, const uint32_t startByteOffset, const uint16_t length)
        : kind_(kind), length_(length), startByteOffset_(startByteOffset) {}

    // Ensure the caller verifies the length fits in uint16_t
    template <typename T>
    explicit Token(TokenKind, uint32_t, T) = delete;

    [[nodiscard]] static Token dummy() {
        static const Token dummy(TokenKind::UNINITIALIZED, 0, static_cast<uint16_t>(0));
        return dummy;
    }

    [[nodiscard]] TokenKind kind() const { return kind_; }

    [[nodiscard]] uint32_t byteOffsetStart() const { return startByteOffset_; }
    [[nodiscard]] uint32_t byteOffsetEnd() const { return startByteOffset_ + length_ - 1; }

    [[nodiscard]] std::string_view lexeme(const std::string_view source) const {
        return {source.data() + startByteOffset_, length_};
    }

   private:
    TokenKind kind_;

    uint16_t length_;
    uint32_t startByteOffset_;
};
