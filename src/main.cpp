#include <chrono>
#include <cstdlib>
#include <filesystem>
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

void show_step(std::string_view message) {
    std::cout << "\033[33m- \033[0m" << message << "..." << std::flush;
}

template <typename F>
decltype(auto) timed(const std::string_view message, F&& f) {
    show_step(message);
    const auto start = Clock::now();

    if constexpr (std::is_void_v<std::invoke_result_t<F>>) {
        std::forward<F>(f)();
        const auto ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count();
        std::cout << "\r\33[2K\033[1;32m✓\033[0m " << message << " (" << ms << " ms)\n";
    } else {
        auto result = std::forward<F>(f)();
        const auto ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count();
        std::cout << "\r\33[2K\033[1;32m✓\033[0m " << message << " (" << ms << " ms)\n";
        return result;
    }
}

void run_or_die(const char* cmd) {
    if (std::system(cmd) != 0) {
        print_error(std::format("Command '{}' failed", cmd));
        std::exit(EXIT_FAILURE);
    }
}

}  // namespace

int main(const int argc, char* argv[]) {
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

    const auto startTime = Clock::now();

    const auto tokens = timed("Lexing", [&] { return Lexer(code).tokenize(); });
    if (opts.logTokens_) {
        for (const auto& token : tokens)
            std::cout << token_kind_to_string(token.kind()) << ": `" << token.lexeme() << "`\n";
    }

    const auto ast = timed("Parsing", [&] { return Parser(tokens).parse(); });
    if (opts.logAst_) AST::log_ast(*ast);

    timed("Semantic analysis", [&] { SemanticAnalyser(*ast).analyse(); });

    const auto assembly = timed("Code generation", [&] { return Generator(*ast).generate(); });
    if (opts.logAssembly_) std::cout << assembly.str();

    run_or_die("rm -rf neutro && mkdir neutro");

    {
        std::ofstream out("neutro/out.asm");
        if (!out) {
            print_error("Could not open output file");
            exit(EXIT_FAILURE);
        }
        out << assembly.str();
    }

    timed("Assembling", [] { run_or_die("nasm -felf64 neutro/out.asm"); });

    const std::filesystem::path runtimePath = std::filesystem::path(PROJECT_ROOT_DIR) / "runtime";
    timed("Assembling runtime", [&] {
        for (const auto& entry : std::filesystem::directory_iterator(runtimePath)) {
            if (entry.path().extension() == ".asm") {
                const std::string src = entry.path().string();
                const std::string name = entry.path().stem().string();  // filename without extension
                const std::string out = "neutro/" + name + ".o";
                run_or_die(("nasm -felf64 " + src + " -o " + out).c_str());
            }
        }
    });

    timed("Linking", [] { run_or_die("ld -o neutro/out neutro/*.o"); });

    std::cout << "== Compiled successfully in " << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - startTime).count() << " ms! ==\n";

    return 0;
}
