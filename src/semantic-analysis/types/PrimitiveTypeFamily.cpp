#include "semantic-analysis/types/PrimitiveTypeFamily.hpp"

const PrimitiveTypeFamily* PrimitiveTypeFamily::familyForType(const PrimitiveKind t) {
    switch (t) {
        case PrimitiveKind::INT:
        case PrimitiveKind::INT8:
        case PrimitiveKind::INT16:
        case PrimitiveKind::INT32:
        case PrimitiveKind::INT64: {
            static const IntegerTypeFamily family{};
            return &family;
        }
        default: {
            static const NoTypeFamily noFamily{};
            return &noFamily;
        }
    }
}