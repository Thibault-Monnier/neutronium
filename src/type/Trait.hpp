#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "ast/Operator.hpp"

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

[[nodiscard]] std::optional<Trait> traitFromOperator(AST::Operator op);

[[nodiscard]] std::string_view traitToString(Trait trait);