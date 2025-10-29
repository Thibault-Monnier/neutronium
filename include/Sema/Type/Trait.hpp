#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "Parser/Operator.hpp"

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

[[nodiscard]] std::string_view trait_to_string(Trait trait);