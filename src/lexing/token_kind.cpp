#include "lexing/token_kind.hpp"

#include <stdexcept>
#include <string>

std::string token_kind_to_string(const TokenKind kind) {
    switch (kind) {
        case TokenKind::IDENTIFIER:
            return "IDENTIFIER";
        case TokenKind::NUMBER_LITERAL:
            return "NUMBER_LITERAL";
        case TokenKind::PLUS:
            return "PLUS";
        case TokenKind::MINUS:
            return "MINUS";
        case TokenKind::STAR:
            return "TIMES";
        case TokenKind::SLASH:
            return "DIVIDE";
        case TokenKind::EQUAL:
            return "EQUAL";
        case TokenKind::LEFT_PAREN:
            return "LEFT_PAREN";
        case TokenKind::RIGHT_PAREN:
            return "RIGHT_PAREN";
        case TokenKind::NEWLINE:
            return "NEWLINE";
        case TokenKind::END_OF_FILE:
            return "END_OF_FILE";
        default:
            throw std::invalid_argument("Invalid token kind passed to token_kind_to_string");
    }
}
