#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "Cli.hpp"
#include "ast/Debug.hpp"
#include "codegen/Generator.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Lexer.hpp"
#include "lex/Token.hpp"
#include "lex/TokenKind.hpp"
#include "parse/Parser.hpp"
#include "sema/SemanticAnalyser.hpp"
#include "source/SourceManager.hpp"
#include "utils/Log.hpp"

using Clock = std::chrono::high_resolution_clock;

namespace {

void printStep(const std::string_view message) {
    std::cout << "\033[33m- \033[0m" << message << "..." << std::flush;
}

template <typename F>
decltype(auto) timed(const std::string_view message, const bool showStep, F&& f) {
    printStep(message);

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

void runOrDie(const std::string& cmd) {
    if (std::system(cmd.c_str()) != 0) {
        printError(std::format("Command '{}' failed", cmd));
        std::exit(EXIT_FAILURE);
    }
}

void compileFile(CompilerOptions opts, SourceManager& sourceManager, const bool verbose) {
    int fileID = -1;
    std::string_view fileContents;

    try {
        timed("Loading source file", verbose, [&] {
            std::tie(fileID, fileContents) =
                sourceManager.loadNewSourceFile(std::move(opts.sourceFilename_));
        });
        opts.sourceFilename_.clear();
    } catch (const std::exception& e) {
        printError(e.what());
        exit(EXIT_FAILURE);
    }

    DiagnosticsEngine diagnosticsEngine(sourceManager, fileID);

    if (opts.logCode_) std::cout << fileContents << '\n';

    const auto tokens =
        timed("Lexing", verbose, [&] { return Lexer(fileContents, diagnosticsEngine).tokenize(); });
    if (opts.logTokens_) {
        const std::string_view filePath = sourceManager.getSourceFilePath(fileID);
        for (const auto& token : tokens) {
            const auto [line, column] =
                sourceManager.getLineColumn(fileID, token.byteOffsetStart());
            std::cout << tokenKindToString(token.kind()) << ": '" << token.lexeme(fileContents)
                      << "' at " << filePath << ":" << line << ":" << column << '\n';
        }
    }

    TypeManager typeManager{diagnosticsEngine};

    const auto ast = timed("Parsing", verbose, [&] {
        return Parser(tokens, diagnosticsEngine, fileContents, typeManager).parse();
    });
    if (opts.logAst_) AST::log_ast(*ast);

    timed("Semantic analysis", verbose, [&] {
        SemanticAnalyser(*ast, opts.targetType_, diagnosticsEngine, typeManager).analyse();
    });

    const auto assembly = timed("Code Generator", verbose, [&] {
        return CodeGen::Generator(*ast, typeManager, opts.targetType_).generate();
    });
    if (opts.logAssembly_) std::cout << assembly.str();

    std::filesystem::path outFilename;
    if (opts.targetType_ == TargetType::EXECUTABLE) {
        outFilename = "neutro/out.asm";
    } else {
        outFilename =
            "neutro/" +
            std::filesystem::path(sourceManager.getSourceFilePath(fileID)).stem().string() + ".asm";
    }

    {
        std::ofstream out(outFilename);
        if (!out) {
            printError("Could not open output file");
            exit(EXIT_FAILURE);
        }
        out << assembly.str();
    }

    const auto asmFilename = outFilename;
    const auto objFilename = outFilename.replace_extension(".o");

    timed("Assembling", verbose, [&] {
        runOrDie(std::format("as --64 {} -o {}", asmFilename.string(), objFilename.string()));
    });
}

}  // namespace

int main(const int argc, const char** argv) {
    CompilerOptions opts = parse_cli(argc, argv);
    const auto startTime = Clock::now();

    runOrDie("rm -rf neutro && mkdir neutro");

    SourceManager sourceManager;

    compileFile(std::move(opts), sourceManager, true);

    const std::filesystem::path runtimePath = std::filesystem::path(PROJECT_ROOT_DIR) / "runtime";

    timed("Building runtime", true, [&] {
        for (const auto& entry : std::filesystem::recursive_directory_iterator(runtimePath)) {
            if (!entry.is_regular_file()) continue;

            const std::string extension = entry.path().extension();
            std::string src = entry.path().string();

            if (extension == ".nt") {
                compileFile(
                    CompilerOptions{
                        .sourceFilename_ = std::move(src),
                        .targetType_ = TargetType::LIBRARY,
                    },
                    sourceManager, false);
            } else if (extension == ".asm") {
                const std::string obj = "neutro/" + entry.path().stem().string() + ".o";

                if (std::filesystem::exists(obj)) {
                    printError(std::format("Duplicate object name would overwrite `{}`", obj));
                    exit(EXIT_FAILURE);
                }

                runOrDie(std::format("as --64 {} -o {}", src, obj));
            }
        }
    });

    timed("Linking", true, [] { runOrDie("ld -o neutro/out neutro/*.o"); });

    std::cout
        << "== Compiled successfully in "
        << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - startTime).count()
        << " ms! ==\n";

    return 0;
}
