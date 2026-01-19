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
    VOID,
};

inline uint16_t defaultTraits(const Kind kind) {
    static_assert(std::is_same_v<std::underlying_type_t<Trait>, uint16_t>,
                  "Trait underlying type must be uint16_t");

    switch (kind) {
        case Kind::INT:
        case Kind::INT8:
        case Kind::INT16:
        case Kind::INT32:
        case Kind::INT64:
            return Trait::ADD | Trait::SUB | Trait::MUL | Trait::DIV | Trait::EQ | Trait::LT |
                   Trait::LTE | Trait::GT | Trait::GTE;
        case Kind::BOOL:
            return Trait::EQ | Trait::NOT;
        case Kind::VOID:
            return 0;
    }

    std::unreachable();
}

}  // namespace Primitive