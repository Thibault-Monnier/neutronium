#include <cctype>
#include <cstdint>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

void print_error(const std::string &message) {
    std::cerr << "\033[31m" << "Error: " << "\033[0m" << message;
}

void print_hint(const std::string &message) {
    std::cout << "\033[33m" << "Hint: " << "\033[0m" << message;
}

enum class TokenType : uint8_t { IDENTIFIER, NUMBER, PLUS, MINUS, END_OF_FILE };

struct Token {
    TokenType type;
    std::optional<std::string> value;
};

std::vector<Token> tokenize(const std::string &source) {
    std::vector<Token> tokens;

    size_t current = 0;

    while (current < source.length()) {
        char c = source[current++];

        if (isspace(c)) {
            continue;
        }

        if (isalpha(c)) {
            std::string identifier;
            identifier += c;
            while (current < source.length() && isalnum(source[current])) {
                identifier += source[current];
                current++;
            }
            tokens.emplace_back(TokenType::IDENTIFIER, identifier);

        } else if (isdigit(c)) {
            std::string number;
            number += c;
            while (current < source.length() && isalnum(source[current])) {
                number += source[current];
                current++;
            }
            tokens.emplace_back(TokenType::NUMBER, number);

        } else if (c == '+') {
            tokens.emplace_back(TokenType::PLUS);
        } else if (c == '-') {
            tokens.emplace_back(TokenType::MINUS);
        }
    }

    tokens.emplace_back(TokenType::END_OF_FILE);

    return tokens;
}

std::string token_type_to_string(TokenType type) {
    switch (type) {
        case TokenType::IDENTIFIER:
            return "IDENTIFIER";
        case TokenType::NUMBER:
            return "NUMBER";
        case TokenType::PLUS:
            return "PLUS";
        case TokenType::MINUS:
            return "MINUS";
        case TokenType::END_OF_FILE:
            return "END_OF_FILE";
        default:
            return "UNKNOWN";
    }
}

int main(const int argc, char *argv[]) {
    if (argc != 2) {
        const std::string errorMessage =
            "Expected 1 argument, got " + std::to_string(argc - 1) + '\n';
        print_error(errorMessage);

        const std::string hintMessage = std::format("Usage: {} <filename>\n", argv[0]);
        print_hint(hintMessage);

        return 1;
    }

    std::ifstream fileStream(argv[1]);
    if (!fileStream.is_open()) {
        const std::string errorMessage = "Could not open file " + std::string(argv[1]) + '\n';
        print_error(errorMessage);
        return 1;
    }

    std::stringstream fileContentsStream;
    fileContentsStream << fileStream.rdbuf();

    fileStream.close();

    std::string fileContents = fileContentsStream.str();

    std::cout << fileContents << '\n';

    std::vector<Token> tokens = tokenize(fileContents);
    for (Token &tok : tokens) {
        std::cout << token_type_to_string(tok.type);
        if (tok.value.has_value()) std::cout << ' ' << tok.value.value();
        std::cout << '\n';
    }

    return 0;
}
