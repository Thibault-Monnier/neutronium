#pragma once

#include <string>

#include "TokenKind.hpp"

class Token {
   public:
    explicit Token(const TokenKind kind, std::string lexeme, const uint32_t byteOffset)
        : kind_(kind), lexeme_(std::move(lexeme)), byteOffset_(byteOffset) {}

    [[nodiscard]] TokenKind kind() const { return kind_; }
    [[nodiscard]] const std::string& lexeme() const { return lexeme_; }

    [[nodiscard]] uint32_t byte_offset_start() const { return byteOffset_; }
    [[nodiscard]] uint32_t byte_offset_end() const {
        return byteOffset_ + static_cast<uint32_t>(lexeme_.size()) - 1;
    }

   private:
    TokenKind kind_;
    std::string lexeme_;

    const uint32_t byteOffset_;
};
