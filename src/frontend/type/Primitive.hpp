#pragma once

#include <cstdint>
#include <type_traits>
#include <utility>

#include "Trait.hpp"

namespace Primitive {

enum class Kind : uint8_t {
    INT,
    INT8,
    INT16,
    INT32,
    INT64,
    BOOL,
    CHAR,
    VOID,
};

inline uint16_t defaultTraits(const Kind kind) {
    static_assert(std::is_same_v<std::underlying_type_t<Trait>, uint16_t>,
                  "Trait underlying type must be uint16_t");

    static constexpr uint16_t RELATIONAL_TRAITS = Trait::LT | Trait::LTE | Trait::GT | Trait::GTE;
    static constexpr uint16_t ARITHMETIC_TRAITS = Trait::ADD | Trait::SUB | Trait::MUL | Trait::DIV;
    static constexpr uint16_t LOGICAL_TRAITS = Trait::NOT | Trait::OR | Trait::AND;

    switch (kind) {
        case Kind::INT:
        case Kind::INT8:
        case Kind::INT16:
        case Kind::INT32:
        case Kind::INT64:
            return ARITHMETIC_TRAITS | RELATIONAL_TRAITS | Trait::EQ;
        case Kind::BOOL:
            return LOGICAL_TRAITS | Trait::EQ;
        case Kind::CHAR:
            return RELATIONAL_TRAITS | Trait::EQ;
        case Kind::VOID:
            return 0;
    }

    std::unreachable();
}

}  // namespace Primitive
