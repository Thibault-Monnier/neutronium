#include <chrono>
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

using Clock = std::chrono::high_resolution_clock;

namespace {

template <typename F>
decltype(auto) timed(std::string_view message, F &&f) {
    const auto start = Clock::now();
    if constexpr (std::is_void_v<std::invoke_result_t<F>>) {
        std::forward<F>(f)();
        const auto ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count();
        std::cout << "\033[1;32m✓\033[0m " << message << " (" << ms << " ms)\n";
    } else {
        auto result = std::forward<F>(f)();
        const auto ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count();
        std::cout << "\033[1;32m✓\033[0m " << message << " (" << ms << " ms)\n";
        return result;
    }
}

void run_or_die(const char *cmd) {
    if (std::system(cmd) != 0) {
        print_error(std::format("Command '{}' failed", cmd));
        std::exit(EXIT_FAILURE);
    }
}

}  // namespace

int main(const int argc, char *argv[]) {
    CompilerOptions opts = parse_cli(argc, argv);

    const std::ifstream source(opts.sourceFilename_);
    if (!source) {
        print_error(std::format("Could not open file '{}'", opts.sourceFilename_));
        exit(EXIT_FAILURE);
    }

    std::stringstream buffer;
    buffer << source.rdbuf();
    const std::string code = buffer.str();

    if (opts.logCode_) std::cout << code << '\n';

    auto lexer = Lexer(code);
    const std::vector<Token> tokens = lexer.tokenize();
    if (opts.logTokens_) {
        for (const auto &token : tokens) {
            std::cout << token_kind_to_string(token.kind()) << ": `" << token.lexeme() << "`\n";
        }
    }

    auto parser = Parser(tokens);
    const auto ast = timed("Parsing", [&] { return parser.parse(); });
    if (opts.logAst_) AST::log_ast(*ast);

    timed("Semantic analysis", [&] { SemanticAnalyser(*ast).analyse(); });

    const auto assembly = timed("Code generation", [&] { return Generator(*ast).generate(); });
    if (opts.logAssembly_) std::cout << assembly.str();

    run_or_die("rm -rf neutro");
    run_or_die("mkdir neutro");

    std::ofstream out("neutro/out.asm");
    if (!out) {
        print_error("Could not open output file");
        exit(EXIT_FAILURE);
    }
    out << assembly.str();

    timed("Assembling", [] { run_or_die("nasm -f elf64 neutro/out.asm -o neutro/out.o"); });
    timed("Linking", [] { run_or_die("ld -o neutro/out neutro/out.o"); });

    std::cout << "== Compiled successfully! ==\n";

    return 0;
}
