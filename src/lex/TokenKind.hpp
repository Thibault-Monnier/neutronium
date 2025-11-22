#pragma once

#include <cstdint>
#include <string>

enum class TokenKind : uint8_t {
    UNINITIALIZED,

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
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,

    // Delimiters
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    COLON,
    SEMICOLON,
    COMMA,
    RIGHT_ARROW,

    // Keywords
    TRUE,
    FALSE,
    LET,
    INT,
    INT8,
    INT16,
    INT32,
    INT64,
    BOOL,
    MUT,
    IF,
    ELIF,
    ELSE,
    WHILE,
    BREAK,
    CONTINUE,
    FN,
    EXTERN,
    EXPORT,
    RETURN,
    EXIT,

    // Miscellaneous
    EOF_,
};

std::string tokenKindToString(TokenKind kind);
