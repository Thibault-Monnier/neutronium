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
    BANG,
    EQUAL,
    EQUAL_EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,

    // Delimiters
    LEFT_PAREN,
    RIGHT_PAREN,
    COLON,
    SEMICOLON,

    // Keywords
    TRUE,
    FALSE,
    LET,
    IF,
    EXIT,

    // Miscellaneous
    END_OF_FILE,
};

std::string token_kind_to_string(TokenKind kind);
