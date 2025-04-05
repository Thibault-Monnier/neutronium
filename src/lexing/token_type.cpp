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
        case TokenType::STAR:
            return "TIMES";
        case TokenType::SLASH:
            return "DIVIDE";
        case TokenType::EQUAL:
            return "EQUAL";
        case TokenType::LEFT_PAREN:
            return "LEFT_PAREN";
        case TokenType::RIGHT_PAREN:
            return "RIGHT_PAREN";
        case TokenType::NEWLINE:
            return "NEWLINE";
        case TokenType::END_OF_FILE:
            return "END_OF_FILE";
        default:
            throw std::invalid_argument("Invalid token type passed to token_type_to_string");
    }
}
