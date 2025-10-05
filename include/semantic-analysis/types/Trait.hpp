#pragma once

#include <cstdint>
#include <optional>

#include "parsing/operator.hpp"

enum class Trait : uint8_t {
    // Arithmetic traits
    ADD,
    SUB,
    MUL,
    DIV,

    // Logical traits
    NOT,

    // Comparison traits
    EQ,
    LT,
    LTE,
    GT,
    GTE,

    SUBSCRIPT,
};

[[nodiscard]] std::optional<Trait> trait_from_operator(AST::Operator op);