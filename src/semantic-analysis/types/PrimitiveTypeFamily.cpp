#include "semantic-analysis/types/PrimitiveTypeFamily.hpp"

const PrimitiveTypeFamily* PrimitiveTypeFamily::familyForType(const PrimitiveType t) {
    switch (t) {
        case PrimitiveType::INT:
        case PrimitiveType::INT8:
        case PrimitiveType::INT16:
        case PrimitiveType::INT32:
        case PrimitiveType::INT64: {
            static const IntegerTypeFamily family{};
            return &family;
        }
        default: {
            static const NoTypeFamily noFamily{};
            return &noFamily;
        }
    }
}