#pragma once

#include <cstdint>
#include <string>

enum class Type : uint8_t {
    INTEGER,
    BOOLEAN,
    EMPTY,
};

std::string type_to_string(const Type type);
