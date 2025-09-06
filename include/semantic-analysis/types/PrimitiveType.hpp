#pragma once

#include <cstdint>

enum class PrimitiveType : uint8_t {
    INT,
    INT8,
    INT16,
    INT32,
    INT64,
    BOOL,
    VOID,
    ANY,
};