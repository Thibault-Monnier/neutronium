#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "lexing/lexer.hpp"
#include "lexing/token.hpp"
#include "lexing/token_type.hpp"
#include "utils/log.hpp"

int main(const int argc, char *argv[]) {
    if (argc != 2) {
        const std::string errorMessage = "Expected 1 argument, got " + std::to_string(argc - 1);
        print_error(errorMessage);

        const std::string hintMessage = std::format("Usage: {} <filename>", argv[0]);
        print_hint(hintMessage);

        exit(EXIT_FAILURE);
    }

    std::ifstream fileStream(argv[1]);
    if (!fileStream.is_open()) {
        const std::string errorMessage = std::format("Could not open file '{}'", argv[1]);
        print_error(errorMessage);
        exit(EXIT_FAILURE);
    }

    std::stringstream fileContentsStream;
    fileContentsStream << fileStream.rdbuf();

    fileStream.close();

    const std::string fileContents = fileContentsStream.str();
    std::cout << fileContents << '\n';

    auto lexer = Lexer(fileContents);
    const std::vector<Token> tokens = lexer.tokenize();
    for (const auto &token : tokens) {
        std::cout << token_type_to_string(token.type());
        if (token.value().has_value()) {
            std::cout << ' ' << token.value().value();
        }
        std::cout << '\n';
    }

    return 0;
}
