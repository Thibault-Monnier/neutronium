#pragma once

#include <cstdint>
#include <string>

enum class TargetType : uint8_t {
    EXECUTABLE,
    LIBRARY,
};

enum class PipelineEndStage : uint8_t {
    LEX,
    PARSE,
    SEMA,
    CODEGEN,
    ALL,
};

struct CompilerOptions {
    bool logCode_ = false;
    bool logTokens_ = false;
    bool logAst_ = false;
    bool logAssembly_ = false;
    std::string sourceFilename_;
    TargetType targetType_ = TargetType::EXECUTABLE;
    PipelineEndStage endStage_ = PipelineEndStage::ALL;
};

CompilerOptions parseCli(int argc, const char** argv);
