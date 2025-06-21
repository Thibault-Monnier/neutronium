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
decltype(auto) timed(const std::string_view message, bool showStep, F&& f) {
    show_step(message);

    const auto start = Clock::now();

    auto printElapsedTime = [&] {
        if (showStep) {
            const auto ms =
                std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - start).count();
            std::cout << "\r\33[2K\033[1;32m✓\033[0m " << message << " (" << ms << " ms)\n";
        }
    };

    if constexpr (std::is_void_v<std::invoke_result_t<F>>) {
        std::forward<F>(f)();
        printElapsedTime();
    } else {
        decltype(auto) result = std::forward<F>(f)();
        printElapsedTime();
        return result;
    }
}

void run_or_die(const std::string& cmd) {
    if (std::system(cmd.c_str()) != 0) {
        print_error(std::format("Command '{}' failed", cmd));
        std::exit(EXIT_FAILURE);
    }
}

void compile_file(const CompilerOptions& opts, bool verbose = true) {
    const std::ifstream source(opts.sourceFilename_);
    if (!source) {
        print_error(std::format("Could not open file '{}'", opts.sourceFilename_));
        exit(EXIT_FAILURE);
    }

    std::stringstream buffer;
    buffer << source.rdbuf();
    const std::string code = buffer.str();

    if (opts.logCode_) std::cout << code << '\n';

    const auto tokens = timed("Lexing", verbose, [&] { return Lexer(code).tokenize(); });
    if (opts.logTokens_) {
        for (const auto& token : tokens)
            std::cout << token_kind_to_string(token.kind()) << ": `" << token.lexeme() << "`\n";
    }

    const auto ast = timed("Parsing", verbose, [&] { return Parser(tokens).parse(); });
    if (opts.logAst_) AST::log_ast(*ast);

    timed("Semantic analysis", verbose,
          [&] { SemanticAnalyser(*ast, opts.targetType_).analyse(); });

    const auto assembly = timed("Code generation", verbose,
                                [&] { return Generator(*ast, opts.targetType_).generate(); });
    if (opts.logAssembly_) std::cout << assembly.str();

    std::string outFilename;
    if (opts.targetType_ == TargetType::EXECUTABLE) {
        outFilename = "neutro/out.asm";
    } else {
        outFilename =
            "neutro/" + std::filesystem::path(opts.sourceFilename_).stem().string() + ".asm";
    }

    {
        std::ofstream out(outFilename);
        if (!out) {
            print_error("Could not open output file");
            exit(EXIT_FAILURE);
        }
        out << assembly.str();
    }

    timed("Assembling", verbose, [&] { run_or_die(std::format("nasm -felf64 {}", outFilename)); });
}

}  // namespace

int main(const int argc, char* argv[]) {
    const CompilerOptions opts = parse_cli(argc, argv);

    const auto startTime = Clock::now();

    run_or_die("rm -rf neutro && mkdir neutro");

    compile_file(opts);

    const std::filesystem::path runtimePath = std::filesystem::path(PROJECT_ROOT_DIR) / "runtime";

    timed("Building runtime", true, [&] {
        for (const auto& entry : std::filesystem::recursive_directory_iterator(runtimePath)) {
            if (!entry.is_regular_file()) continue;

            const std::string extension = entry.path().extension();
            const std::string src = entry.path().string();

            if (extension == ".nt") {
                compile_file(
                    CompilerOptions{
                        .logCode_ = opts.logCode_,
                        .logTokens_ = opts.logTokens_,
                        .logAst_ = opts.logAst_,
                        .logAssembly_ = opts.logAssembly_,
                        .sourceFilename_ = src,
                        .targetType_ = TargetType::LIBRARY,
                    },
                    false);
            } else if (extension == ".asm") {
                const std::string obj = "neutro/" + entry.path().stem().string() + ".o";

                if (std::filesystem::exists(obj)) {
                    print_error(std::format("Duplicate object name would overwrite `{}`", obj));
                    exit(EXIT_FAILURE);
                }

                run_or_die(std::format("nasm -felf64 {} -o {}", src, obj));
            }
        }
    });

    timed("Linking", true, [] { run_or_die("ld -o neutro/out neutro/*.o"); });

    std::cout
        << "== Compiled successfully in "
        << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - startTime).count()
        << " ms! ==\n";

    return 0;
}
