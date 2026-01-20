#include <sched.h>

#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <print>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>

#include "Cli.hpp"
#include "ast/AST.hpp"
#include "ast/ASTArena.hpp"
#include "ast/Debug.hpp"
#include "codegen/Generator.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Lexer.hpp"
#include "lex/Token.hpp"
#include "lex/TokenKind.hpp"
#include "parse/Parser.hpp"
#include "sema/SemanticAnalyser.hpp"
#include "source/FileID.hpp"
#include "source/SourceManager.hpp"
#include "type/TypeManager.hpp"
#include "utils/Log.hpp"

using Clock = std::chrono::high_resolution_clock;

namespace {

class Stage {
   public:
    explicit Stage(const std::string_view message, const bool verbose)
        : message_(message), verbose_(verbose) {
        if (!verbose_) return;

        printStepStart();
        startTime_ = Clock::now();
    }

    ~Stage() {
        if (!verbose_) return;

        const auto end = Clock::now();
        const auto duration =
            std::chrono::duration_cast<std::chrono::milliseconds>(end - startTime_).count();

        try {
            printStepEnd(duration);
        } catch (...) {
        }
    }

   private:
    std::string_view message_;
    Clock::time_point startTime_;
    bool verbose_;

    void printStepStart() const {
        std::print("\033[33m- \033[0m{}...", message_.data());
        std::cout.flush();
    }

    void printStepEnd(const std::int64_t duration) const {
        std::print("\r\33[2K\033[1;32m✓\033[0m {} ({} ms)\n", message_.data(), duration);
    }
};

void runOrDie(const std::string& cmd) {
    if (std::system(cmd.c_str()) != 0) {
        printError(std::format("Command '{}' failed", cmd));
        std::exit(EXIT_FAILURE);
    }
}

void compileFile(CompilerOptions opts, SourceManager& sourceManager, const bool verbose) {
    FileID fileID = 0;
    std::string_view fileContents;

    try {
        {
            const Stage stage("Loading source file", verbose);
            std::tie(fileID, fileContents) =
                sourceManager.loadNewSourceFile(std::move(opts.sourceFilename_));
        }

        opts.sourceFilename_.clear();
    } catch (const std::exception& e) {
        printError(e.what());
        std::exit(EXIT_FAILURE);
    }

    DiagnosticsEngine diagnosticsEngine(sourceManager);

    if (opts.logCode_) std::cout << fileContents << '\n';

    if (opts.endStage_ == PipelineEndStage::LEX) {
        {
            const Stage stage("Lexing", verbose);

            Lexer lexer(fileContents, diagnosticsEngine, fileID);
            Token token = lexer.lex();
            if (opts.logTokens_) std::print("{}\n", tokenKindToString(token.kind()));
            while (token.kind() != TokenKind::EOF_) {
                token = lexer.lex();
                if (opts.logTokens_) std::print("{}\n", tokenKindToString(token.kind()));
            }
        }

        return;
    }

    ASTArena astArena;
    TypeManager typeManager(diagnosticsEngine);

    AST::CompilationUnit* const ast = [&] {
        const Stage stage("Parsing", verbose);

        Parser parser(diagnosticsEngine, fileID, fileContents, astArena, typeManager);
        return parser.parse();
    }();

    if (opts.logAst_) AST::logAst(*ast);
    if (opts.endStage_ == PipelineEndStage::PARSE) return;

    {
        const Stage stage("Semantic Analysis", verbose);

        SemanticAnalyser sema(*ast, opts.targetType_, diagnosticsEngine, fileID, typeManager);
        sema.analyse();
    }

    if (opts.endStage_ == PipelineEndStage::SEMA) return;

    const auto assembly = [&] {
        const Stage stage("Code Generation", verbose);

        CodeGen::Generator generator(*ast, typeManager, opts.targetType_);
        return generator.generate();
    }();

    if (opts.logAssembly_) std::cout << assembly.str();
    if (opts.endStage_ == PipelineEndStage::CODEGEN) return;

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
            std::exit(EXIT_FAILURE);
        }
        out << assembly.str();
    }

    const std::filesystem::path asmFilename = outFilename;
    const std::filesystem::path objFilename = outFilename.replace_extension(".o");

    {
        const Stage stage("Assembling", verbose);

        runOrDie(std::format("as --64 {} -o {}", asmFilename.string(), objFilename.string()));
    }
}

void compileRuntime(SourceManager& sourceManager) {
    const std::filesystem::path runtimePath = std::filesystem::path(PROJECT_ROOT_DIR) / "runtime";

    {
        const Stage stage("Building runtime", true);

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
                    std::exit(EXIT_FAILURE);
                }

                runOrDie(std::format("as --64 {} -o {}", src, obj));
            }
        }
    }
}

void link() {
    const Stage stage("Linking", true);
    runOrDie("ld -o neutro/out neutro/*.o");
}

}  // namespace

int main(const int argc, const char** argv) {
    const CompilerOptions opts = parseCli(argc, argv);
    const auto startTime = Clock::now();

    runOrDie("rm -rf neutro && mkdir neutro");

    SourceManager sourceManager;

    compileFile(opts, sourceManager, true);

    if (opts.endStage_ == PipelineEndStage::ALL) {
        compileRuntime(sourceManager);
        link();
    }

    std::cout
        << "== Compiled successfully in "
        << std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - startTime).count()
        << " ms! ==\n";

    return 0;
}
