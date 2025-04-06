#pragma once

#include <cstdint>
#include <string>

enum class TokenKind: uint8_t { IDENTIFIER, NUMBER_LITERAL, PLUS, MINUS, STAR, SLASH, EQUAL, LEFT_PAREN, RIGHT_PAREN, NEWLINE, END_OF_FILE };

std::string token_kind_to_string(TokenKind kind);
