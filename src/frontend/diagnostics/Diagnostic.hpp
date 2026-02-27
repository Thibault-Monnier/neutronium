#pragma once

#include <cstdint>
#include <string>

#include "source/FileID.hpp"

struct Diagnostic {
    enum class Level : uint8_t { ERROR };

    const std::string message_;
    const uint32_t byteOffsetStart_;
    const uint32_t byteOffsetEnd_;
    const FileID fileID_;
    const Level level_;
};
