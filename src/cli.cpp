#include "../include/cli.hpp"

#include <cxxopts.hpp>
#include <format>
#include <iostream>

#include "utils/log.hpp"

CompilerOptions parse_cli(int argc, char** argv) {
    cxxopts::Options options(argv[0], "Neutronium language compiler");
    options.positional_help("<input-file>").show_positional_help();

    options.add_options()("h,help", "Print help")("v,version", "Print the compiler version")(
        "target-type", "Compilation target: bin (executable) or lib (library)",
        cxxopts::value<std::string>()->default_value("bin"))("d,debug", "Enable all debug logs")(
        "log-code", "Log source code processing")("log-tokens", "Log token stream")(
        "log-ast", "Log AST construction")("log-assembly", "Log final assembly output")(
        "log", "Comma-separated logs (code,tokens,ast,assembly)",
        cxxopts::value<std::vector<std::string>>()->implicit_value(""))(
        "input", "Source file", cxxopts::value<std::string>());

    options.parse_positional({"input"});

    cxxopts::ParseResult result;
    try {
        result = options.parse(argc, argv);
    } catch (const std::exception& e) {
        print_error(e.what());
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }

    if (result.count("help")) {
        std::cout << options.help() << '\n';
        std::exit(EXIT_SUCCESS);
    }

    if (result.count("version")) {
        std::cout << "Neutronium Compiler v2.0.0\n";
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
        print_error(std::format("Unknown target type: '{}'", targetType));
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }

    if (result.count("debug")) {
        opts.logCode_ = opts.logTokens_ = opts.logAst_ = opts.logAssembly_ = true;
    }

    if (result.count("log")) {
        const auto logs = result["log"].as<std::vector<std::string>>();
        for (const auto& item : logs) {
            if (item.empty()) continue;  // implicit empty value
            if (item == "code")
                opts.logCode_ = true;
            else if (item == "tokens")
                opts.logTokens_ = true;
            else if (item == "ast")
                opts.logAst_ = true;
            else if (item == "assembly")
                opts.logAssembly_ = true;
            else {
                print_error(std::format("Unknown log type: '{}'", item));
                std::cout << '\n' << options.help() << '\n';
                std::exit(EXIT_FAILURE);
            }
        }
    }

    if (!result.count("input")) {
        print_error("Missing input file");
        std::cout << '\n' << options.help() << '\n';
        std::exit(EXIT_FAILURE);
    }
    opts.sourceFilename_ = result["input"].as<std::string>();

    return opts;
}
