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
            return "STAR";
        case TokenKind::SLASH:
            return "SLASH";
        case TokenKind::BANG:
            return "BANG";
        case TokenKind::EQUAL:
            return "EQUAL";
        case TokenKind::EQUAL_EQUAL:
            return "EQUAL_EQUAL";
        case TokenKind::NOT_EQUAL:
            return "NOT_EQUAL";
        case TokenKind::LESS_THAN:
            return "LESS_THAN";
        case TokenKind::LESS_THAN_EQUAL:
            return "LESS_THAN_EQUAL";
        case TokenKind::GREATER_THAN:
            return "GREATER_THAN";
        case TokenKind::GREATER_THAN_EQUAL:
            return "GREATER_THAN_EQUAL";

        case TokenKind::LEFT_PAREN:
            return "LEFT_PAREN";
        case TokenKind::RIGHT_PAREN:
            return "RIGHT_PAREN";
        case TokenKind::LEFT_BRACE:
            return "LEFT_BRACE";
        case TokenKind::RIGHT_BRACE:
            return "RIGHT_BRACE";
        case TokenKind::COLON:
            return "COLON";
        case TokenKind::SEMICOLON:
            return "SEMICOLON";

        case TokenKind::TRUE:
            return "TRUE";
        case TokenKind::FALSE:
            return "FALSE";
        case TokenKind::LET:
            return "LET";
        case TokenKind::IF:
            return "IF";
        case TokenKind::EXIT:
            return "EXIT";

        case TokenKind::END_OF_FILE:
            return "END_OF_FILE";
        default:
            throw std::invalid_argument("Invalid token kind passed to token_kind_to_string");
    }
}
