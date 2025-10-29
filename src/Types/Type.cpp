#include "Type/Type.hpp"

#include <magic_enum/magic_enum.hpp>

#include "Type/TypeManager.hpp"

const Type& Type::integerFamilyType() {
    static const Type instance(Primitive::Kind::INT, true);
    return instance;
}

const Type& Type::anyFamilyType() {
    static const Type instance(Primitive::Kind::VOID, &AnyTypeFamily::getInstance(),
                               TypeKind::UNKNOWN);
    return instance;
}

int Type::sizeBits(const TypeManager& typeManager) const {
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
            const Type& arrayElement = typeManager.getType(arrayElementTypeID_);
            return arrayElement.sizeBits(typeManager) * static_cast<int>(arrayLength_);
        }

        case TypeKind::UNKNOWN:
            std::unreachable();
    }
    std::unreachable();
}

std::string Type::to_string(const TypeManager& typeManager) const {
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
            const Type& arrayElement = typeManager.getType(arrayElementTypeID_);
            result += "array[" + arrayElement.to_string(typeManager) + " * " +
                      std::to_string(arrayLength_) + "]";
            break;
        }

        case TypeKind::UNKNOWN:
            result += "unknown";
            break;
    }

    if (family_->kind() != PrimitiveTypeFamily::Kind::NONE) {
        const std::string_view familyKind = magic_enum::enum_name(family_->kind());
        result += " ( " + std::string(familyKind) + " family )";
    }

    return result;
}

TypeID Type::array_element_type_id() const {
    if (kind_ == TypeKind::ARRAY) {
        return arrayElementTypeID_;
    }
    std::unreachable();
}

bool Type::mergeWith(const Type& other, const TypeManager& typeManager) {
    if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) std::unreachable();

    if (!matches(other, typeManager)) return false;

    // We only need to modify `this`
    if (family_->kind() == PrimitiveTypeFamily::Kind::NONE || other.family_->isInFamily(primitive_))
        return true;

    if (kind_ == TypeKind::UNKNOWN || other.family_->kind() == PrimitiveTypeFamily::Kind::NONE ||
        family_->isInFamily(other.primitive_)) {
        *this = other;
        return true;
    }

    std::unreachable();
}

bool Type::matches(const Type& other, const TypeManager& typeManager) const {
    if (family_->isInFamily(other.primitive_) || other.family_->isInFamily(primitive_)) {
        return true;
    }

    if (kind_ != other.kind_) return false;
    if (traits_ != other.traits_) return false;

    switch (kind_) {
        case TypeKind::PRIMITIVE:
            // If one of the Type has no family, it is fully determined, so we just need to make
            // sure the other type is compatible.
            if (family_->kind() == PrimitiveTypeFamily::Kind::NONE &&
                other.family_->kind() == PrimitiveTypeFamily::Kind::NONE) {
                return primitive_ == other.primitive_;
            }

            // Otherwise, we checked that the families are not compatible above.
            return false;

        case TypeKind::ARRAY: {
            const Type& arrayElement = typeManager.getType(arrayElementTypeID_);
            const Type& otherArrayElement = typeManager.getType(other.arrayElementTypeID_);
            return arrayLength_ == other.arrayLength_ &&
                   arrayElement.matches(otherArrayElement, typeManager);
        }
        case TypeKind::UNKNOWN:
            return true;
    }

    std::unreachable();
}
