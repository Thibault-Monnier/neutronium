#pragma once

#include <cstdint>
#include <string>

enum class TargetType : uint8_t {
    EXECUTABLE,
    LIBRARY,
};

struct CompilerOptions {
    bool logCode_ = false;
    bool logTokens_ = false;
    bool logAst_ = false;
    bool logAssembly_ = false;
    std::string sourceFilename_;
    TargetType targetType_ = TargetType::EXECUTABLE;
};

CompilerOptions parse_cli(int argc, char** argv);
