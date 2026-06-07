#include "Type.hpp"

#include <cassert>
#include <cstdint>
#include <magic_enum/magic_enum.hpp>
#include <string>
#include <string_view>
#include <utility>

#include "Primitive.hpp"
#include "TypeManager.hpp"

uint32_t Type::sizeBits(const TypeManager& typeManager) const {
    switch (kind_) {
        case TypeKind::PRIMITIVE: {
            switch (primitive_) {
                case Primitive::Kind::INT8:
                case Primitive::Kind::BOOL:
                    return 8;
                case Primitive::Kind::INT16:
                    return 16;
                case Primitive::Kind::INT32:
                    return 32;
                case Primitive::Kind::INT:
                case Primitive::Kind::INT64:
                    return 64;
                case Primitive::Kind::VOID:
                    return 0;
            }
            break;
        }

        case TypeKind::ARRAY: {
            const Type& arrayElement = typeManager.getTypeResolved(arrayElementTypeID_);
            return arrayElement.sizeBits(typeManager) * arrayLength_;
        }
    }
    std::unreachable();
}

std::string Type::toString(const TypeManager& typeManager) const {
    std::string result;
    switch (kind_) {
        case TypeKind::PRIMITIVE: {
            switch (primitive_) {
                case Primitive::Kind::INT:
                    result += "int";
                    break;
                case Primitive::Kind::INT8:
                    result += "int8";
                    break;
                case Primitive::Kind::INT16:
                    result += "int16";
                    break;
                case Primitive::Kind::INT32:
                    result += "int32";
                    break;
                case Primitive::Kind::INT64:
                    result += "int64";
                    break;
                case Primitive::Kind::BOOL:
                    result += "bool";
                    break;
                case Primitive::Kind::VOID:
                    result += "void";
                    break;
            }
            break;
        }

        case TypeKind::ARRAY: {
            const Type* arrayElement = typeManager.getType(arrayElementTypeID_);
            const std::string elemStr =
                arrayElement ? arrayElement->toString(typeManager) : "unknown";

            result += "array[" + elemStr + " * " + std::to_string(arrayLength_) + "]";
            break;
        }
    }

    if (family()->kind() != PrimitiveTypeFamily::Kind::NONE) {
        const std::string_view familyKind = magic_enum::enum_name(family()->kind());
        result += " ( " + std::string(familyKind) + " family )";
    }

    return result;
}

bool Type::mergeWith(const Type& other) {
    if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) std::unreachable();

    if (!matches(other)) return false;

    // We only need to modify `this`
    if (!hasFamily_ || other.family()->isInFamily(primitive_)) return true;

    assert(!other.hasFamily_ || family()->isInFamily(other.primitive_));

    *this = other;
    return true;
}

bool Type::matches(const Type& other) const {
    if (family()->isInFamily(other.primitive_) || other.family()->isInFamily(primitive_)) {
        return true;
    }

    if (kind_ != other.kind_) return false;
    if (traits_ != other.traits_) return false;

    switch (kind_) {
        case TypeKind::PRIMITIVE:
            // If one of the types has no family, it is fully determined, so we just need to make
            // sure the other type is compatible.
            if (!hasFamily_ && !other.hasFamily_) {
                return primitive_ == other.primitive_;
            }
            // Otherwise, we checked that the families are not compatible above.
            return false;

        case TypeKind::ARRAY:
            return arrayLength_ == other.arrayLength_;
    }

    std::unreachable();
}
