#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "cli.hpp"
#include "generation/generator.hpp"
#include "lexing/lexer.hpp"
#include "lexing/token.hpp"
#include "lexing/token_kind.hpp"
#include "parsing/parser.hpp"
#include "semantic-analysis/semantic_analyser.hpp"
#include "utils/log.hpp"

int main(const int argc, char *argv[]) {
    CompilerOptions opts = parse_cli(argc, argv);

    std::ifstream fileStreamIn(opts.sourceFilename_);
    if (!fileStreamIn.is_open()) {
        const std::string errorMessage =
            std::format("Could not open file '{}'", opts.sourceFilename_);
        print_error(errorMessage);
        exit(EXIT_FAILURE);
    }
    std::stringstream fileContentsStream;
    fileContentsStream << fileStreamIn.rdbuf();
    fileStreamIn.close();

    const std::string fileContents = fileContentsStream.str();
    if (opts.logCode_) std::cout << fileContents << '\n';

    auto lexer = Lexer(fileContents);
    const std::vector<Token> tokens = lexer.tokenize();
    if (opts.logTokens_) {
        for (const auto &token : tokens) {
            std::cout << token_kind_to_string(token.kind()) << ": `" << token.lexeme() << "`\n";
        }
    }

    auto parser = Parser(tokens);
    const auto ast = parser.parse();
    if (opts.logAst_) AST::log_ast(*ast);

    auto semanticAnalyser = SemanticAnalyser(*ast);
    semanticAnalyser.analyse();

    auto generator = Generator(*ast);
    const auto assemblyCode = generator.generate();
    if (opts.logAssembly_) std::cout << assemblyCode.str();

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
