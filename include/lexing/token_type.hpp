#pragma once

#include <cstdint>
#include <string>

enum class TokenType : uint8_t { IDENTIFIER, NUMBER, PLUS, MINUS, END_OF_FILE };

std::string token_type_to_string(TokenType type);