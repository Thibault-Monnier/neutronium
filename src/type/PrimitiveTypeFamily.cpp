#include "PrimitiveTypeFamily.hpp"

#include "Primitive.hpp"

const PrimitiveTypeFamily* PrimitiveTypeFamily::familyForType(const Primitive::Kind t) {
    switch (t) {
        case Primitive::Kind::INT:
        case Primitive::Kind::INT8:
        case Primitive::Kind::INT16:
        case Primitive::Kind::INT32:
        case Primitive::Kind::INT64: {
            return &IntegerTypeFamily::getInstance();
        }
        case Primitive::Kind::UNKNOWN: {
            return &AnyTypeFamily::getInstance();
        }
        default: {
            return &NoTypeFamily::getInstance();
        }
    }
}