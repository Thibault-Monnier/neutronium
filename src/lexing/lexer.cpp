#include "lexing/lexer.hpp"

#include <format>
#include <string>
#include <vector>

#include "lexing/token.hpp"
#include "lexing/token_type.hpp"
#include "utils/log.hpp"

std::vector<Token> tokenize(const std::string &source) {
    std::vector<Token> tokens;

    size_t current = 0;

    while (current < source.length()) {
        char c = source.at(current++);

        if (isspace(c)) {
            continue;
        }

        if (isalpha(c)) {
            std::string identifier;
            identifier += c;
            while (current < source.length() && isalnum(source.at(current))) {
                identifier += source.at(current);
                current++;
            }
            tokens.emplace_back(TokenType::IDENTIFIER, identifier);

        } else if (isdigit(c)) {
            std::string number;
            number += c;
            while (current < source.length() && isalnum(source.at(current))) {
                number += source.at(current);
                current++;
            }
            tokens.emplace_back(TokenType::NUMBER, number);

        } else if (c == '+') {
            tokens.emplace_back(TokenType::PLUS);
        } else if (c == '-') {
            tokens.emplace_back(TokenType::MINUS);
        } else {
            const std::string errorMessage = std::format(
                "Invalid character at index {}, got '{}' at beginning of word\n", current - 1, c);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }
    }

    tokens.emplace_back(TokenType::END_OF_FILE);

    return tokens;
}