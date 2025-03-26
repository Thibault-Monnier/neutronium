#include "lexing/token_type.hpp"

#include <stdexcept>
#include <string>

std::string token_type_to_string(const TokenType type) {
    switch (type) {
        case TokenType::IDENTIFIER:
            return "IDENTIFIER";
        case TokenType::NUMBER:
            return "NUMBER";
        case TokenType::PLUS:
            return "PLUS";
        case TokenType::MINUS:
            return "MINUS";
        case TokenType::NEWLINE:
            return "NEWLINE";
        case TokenType::END_OF_FILE:
            return "END_OF_FILE";
        default:
            throw std::invalid_argument("Invalid token type");
    }
}