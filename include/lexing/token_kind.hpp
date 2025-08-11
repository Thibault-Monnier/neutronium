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
    CONST,
    LET,
    INT,
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

std::string token_kind_to_string(TokenKind kind);
