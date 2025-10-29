#include "Sema/Type/PrimitiveTypeFamily.hpp"

const PrimitiveTypeFamily* PrimitiveTypeFamily::familyForType(const Primitive::Kind t) {
    switch (t) {
        case Primitive::Kind::INT:
        case Primitive::Kind::INT8:
        case Primitive::Kind::INT16:
        case Primitive::Kind::INT32:
        case Primitive::Kind::INT64: {
            static const IntegerTypeFamily family{};
            return &family;
        }
        default: {
            static const NoTypeFamily noFamily{};
            return &noFamily;
        }
    }
}