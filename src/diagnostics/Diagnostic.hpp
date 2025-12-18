#pragma once

#include <cstdint>
#include <string>

struct Diagnostic {
    enum class Level : uint8_t { ERROR };

    const std::string message_;
    const uint32_t byteOffsetStart_;
    const uint32_t byteOffsetEnd_;
    Level level_;
};
