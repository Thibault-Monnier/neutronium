#pragma once

#include <cstdint>
#include <utility>
#include <vector>

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

inline std::vector<Trait> defaultTraits(const Kind kind) {
    switch (kind) {
        case Kind::INT:
        case Kind::INT8:
        case Kind::INT16:
        case Kind::INT32:
        case Kind::INT64:
            return std::vector{Trait::ADD, Trait::SUB, Trait::MUL, Trait::DIV, Trait::EQ,
                               Trait::LT,  Trait::LTE, Trait::GT,  Trait::GTE};
        case Kind::BOOL:
            return std::vector{Trait::EQ, Trait::NOT};
        case Kind::VOID:
            return std::vector<Trait>{};
    }

    std::unreachable();
}

}  // namespace Primitive