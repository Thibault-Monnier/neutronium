#pragma once

#include <string>

struct CompilerOptions {
    bool logCode_ = false;
    bool logTokens_ = false;
    bool logAst_ = false;
    bool logAssembly_ = false;
    std::string sourceFilename_;
};

CompilerOptions parse_cli(int argc, char** argv);
