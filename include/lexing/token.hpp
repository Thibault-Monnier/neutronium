#pragma once

#include <optional>
#include <string>

#include "token_type.hpp"

struct Token {
    explicit Token(const TokenType type, std::string val) : type_(type), value_(std::move(val)) {}
    explicit Token(const TokenType type) : type_(type), value_(std::nullopt) {}

    [[nodiscard]] TokenType type() const { return type_; }
    [[nodiscard]] const std::optional<std::string> &value() const { return value_; }

   private:
    TokenType type_;
    std::optional<std::string> value_;
};