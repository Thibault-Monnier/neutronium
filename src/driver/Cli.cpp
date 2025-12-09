#include "Cli.hpp"

#include <cstdlib>
#include <cxxopts.hpp>
#include <exception>
#include <format>
#include <iostream>
#include <vector>

#include "utils/Log.hpp"

CompilerOptions parseCli(const int argc, const char** argv) {
    cxxopts::Options options(argv[0], "Neutronium language compiler");
    options.positional_help("<input-file>").show_positional_help();

    options.add_options()("h,help", "Print help")("v,version", "Print the compiler version")(
        "target-type", "Compilation target: bin (executable) or lib (library)",
        cxxopts::value<std::string>()->default_value("bin"))("d,debug", "Enable all debug logs")(
        "log-code", "Log the raw source file")("log-tokens", "Log the generated tokens")(
        "log-ast", "Log the generated AST")("log-assembly", "Log the final assembly output")(
        "log", "Comma-separated logs (code,ast,assembly)",
        cxxopts::value<std::vector<std::string>>()->implicit_value(""))(
        "input", "Specify the source file", cxxopts::value<std::string>())(
        "only-lex", "Run the lexer, then stop")("only-parse", "Run the parser, then stop")(
        "only-sema", "Run semantic analysis, then stop")("only-codegen",
                                                         "Run code generation, then stop");

    options.parse_positional({"input"});

    cxxopts::ParseResult result;
    try {
        result = options.parse(argc, argv);
    } catch (const std::exception& e) {
        printError(e.what());
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }

    if (result.count("help")) {
        std::cout << options.help() << '\n';
        std::exit(EXIT_SUCCESS);
    }

    if (result.count("version")) {
        std::cout << "Neutronium Compiler v0.3.0\n";
        std::exit(EXIT_SUCCESS);
    }

    CompilerOptions opts;
    opts.logCode_ = result.count("log-code");
    opts.logTokens_ = result.count("log-tokens");
    opts.logAst_ = result.count("log-ast");
    opts.logAssembly_ = result.count("log-assembly");

    const std::string targetType = result["target-type"].as<std::string>();
    if (targetType == "bin" || targetType == "executable") {
        opts.targetType_ = TargetType::EXECUTABLE;
    } else if (targetType == "lib" || targetType == "library") {
        opts.targetType_ = TargetType::LIBRARY;
    } else {
        printError(std::format("Unknown target type: '{}'", targetType));
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }

    if (result.count("debug")) {
        opts.logCode_ = opts.logAst_ = opts.logAssembly_ = true;
    }

    if (result.count("log")) {
        const auto logs = result["log"].as<std::vector<std::string>>();
        for (const auto& item : logs) {
            if (item.empty()) continue;  // implicit empty value
            if (item == "code")
                opts.logCode_ = true;
            else if (item == "ast")
                opts.logAst_ = true;
            else if (item == "assembly")
                opts.logAssembly_ = true;
            else {
                printError(std::format("Unknown log type: '{}'", item));
                std::cout << '\n' << options.help() << '\n';
                std::exit(EXIT_FAILURE);
            }
        }
    }

    if (!result.count("input")) {
        printError("Missing input file");
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }
    opts.sourceFilename_ = result["input"].as<std::string>();

    if (result.count("only-lex")) {
        opts.endStage_ = PipelineEndStage::LEX;
    } else if (result.count("only-parse")) {
        opts.endStage_ = PipelineEndStage::PARSE;
    } else if (result.count("only-sema")) {
        opts.endStage_ = PipelineEndStage::SEMA;
    } else if (result.count("only-codegen")) {
        opts.endStage_ = PipelineEndStage::CODEGEN;
    }

    if (opts.logTokens_ && opts.endStage_ != PipelineEndStage::LEX) {
        printWarning("Token logging requires the --only-lex option");
    }

    return opts;
}
