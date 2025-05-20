#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "generation/generator.hpp"
#include "lexing/lexer.hpp"
#include "lexing/token.hpp"
#include "lexing/token_kind.hpp"
#include "parsing/parser.hpp"
#include "semantic-analysis/semantic_analyser.hpp"
#include "utils/log.hpp"

int main(const int argc, char *argv[]) {
    if (argc != 2) {
        const std::string errorMessage = "Expected 1 argument, got " + std::to_string(argc - 1);
        print_error(errorMessage);

        const std::string hintMessage = std::format("Usage: {} <filename>", argv[0]);
        print_hint(hintMessage);

        exit(EXIT_FAILURE);
    }

    std::ifstream fileStreamIn(argv[1]);
    if (!fileStreamIn.is_open()) {
        const std::string errorMessage = std::format("Could not open file '{}'", argv[1]);
        print_error(errorMessage);
        exit(EXIT_FAILURE);
    }
    std::stringstream fileContentsStream;
    fileContentsStream << fileStreamIn.rdbuf();
    fileStreamIn.close();

    const std::string fileContents = fileContentsStream.str();
    std::cout << fileContents << '\n';

    auto lexer = Lexer(fileContents);
    const std::vector<Token> tokens = lexer.tokenize();
    for (const auto &token : tokens) {
        std::cout << token_kind_to_string(token.kind()) << ": `" << token.lexeme() << "`\n";
    }

    auto parser = Parser(tokens);
    const auto ast = parser.parse();

    auto semanticAnalyser = SemanticAnalyser(*ast);
    const auto symbolTable = semanticAnalyser.analyse();

    auto generator = Generator(*ast, symbolTable);
    const auto assemblyCode = generator.generate();

    auto runOrDie = [](const char *cmd) {
        int rc = std::system(cmd);
        if (rc != 0) {
            print_error(std::format("Command '{}' failed (exit {})", cmd, rc));
            exit(EXIT_FAILURE);
        }
    };

    runOrDie("rm -rf neutro");
    runOrDie("mkdir neutro");

    std::ofstream fileStreamOut("neutro/out.asm");
    if (!fileStreamOut.is_open()) {
        print_error("Could not open output file");
        exit(EXIT_FAILURE);
    }
    fileStreamOut << assemblyCode.str();
    fileStreamOut.close();

    runOrDie("nasm -felf64 neutro/out.asm");
    runOrDie("ld -o neutro/out neutro/out.o");

    return 0;
}
