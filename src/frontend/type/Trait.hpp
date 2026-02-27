#pragma once

#include <cstdint>
#include <magic_enum/magic_enum.hpp>
#include <optional>
#include <string_view>
#include <type_traits>

#include "frontend/ast/Operator.hpp"

enum class Trait : uint16_t {
    // Arithmetic traits
    ADD = 1 << 0,
    SUB = 1 << 1,
    MUL = 1 << 2,
    DIV = 1 << 3,

    // Logical traits
    NOT = 1 << 4,
    OR = 1 << 5,
    AND = 1 << 6,

    // Comparison traits
    EQ = 1 << 7,
    LT = 1 << 8,
    LTE = 1 << 9,
    GT = 1 << 10,
    GTE = 1 << 11,

    SUBSCRIPT = 1 << 12,
};

template <>
struct magic_enum::customize::enum_range<Trait> {
    static constexpr int min = 0;
    static constexpr int max = static_cast<int>(Trait::SUBSCRIPT);

    static_assert(min < max && (max - min) <= UINT16_MAX);  // Magic enum requirement
};

inline uint16_t operator|(Trait a, Trait b) {
    return static_cast<uint16_t>(a) | static_cast<uint16_t>(b);
}

inline uint16_t operator|(const std::underlying_type_t<Trait> a, Trait b) {
    return a | static_cast<std::underlying_type_t<Trait>>(b);
}

[[nodiscard]] std::optional<Trait> traitFromOperator(AST::Operator op);

[[nodiscard]] std::string_view traitToString(Trait trait);