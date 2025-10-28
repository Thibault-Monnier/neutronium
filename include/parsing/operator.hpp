#pragma once

#include <cstdint>

namespace AST {

enum class Operator : uint8_t {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    LOGICAL_NOT,
    ASSIGN,
    ADD_ASSIGN,
    SUBTRACT_ASSIGN,
    MULTIPLY_ASSIGN,
    DIVIDE_ASSIGN,
    EQUALS,
    NOT_EQUALS,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    GREATER_THAN,
    GREATER_THAN_OR_EQUAL,
    UNDEFINED_OPERATOR,
};

}