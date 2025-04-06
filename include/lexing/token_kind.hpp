#pragma once

#include <cstdint>
#include <string>

enum class TokenKind : uint8_t {
    // Primary tokens
    IDENTIFIER,
    NUMBER_LITERAL,

    // Operators
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL,

    // Delimiters
    LEFT_PAREN,
    RIGHT_PAREN,
    NEWLINE,

    // Keywords
    LET,

    // Miscellaneous
    END_OF_FILE,
};

std::string token_kind_to_string(TokenKind kind);
