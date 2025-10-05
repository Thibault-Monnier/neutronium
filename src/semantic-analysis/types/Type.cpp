#include "semantic-analysis/types/Type.hpp"

#include "magic_enum.hpp"

const Type& Type::integerFamilyType() {
    static const Type instance(PrimitiveKind::INT, true);
    return instance;
}

const Type& Type::anyFamilyType() {
    static const Type instance(PrimitiveKind::VOID, &AnyTypeFamily::getInstance(),
                               TypeKind::UNKNOWN);
    return instance;
}

int Type::sizeBytes() const {
    switch (kind_) {
        case TypeKind::PRIMITIVE: {
            switch (primitive_) {
                case PrimitiveKind::INT8:
                case PrimitiveKind::BOOL:
                    return 1;
                case PrimitiveKind::INT16:
                    return 2;
                case PrimitiveKind::INT32:
                    return 4;
                case PrimitiveKind::INT:
                case PrimitiveKind::INT64:
                    return 8;
                case PrimitiveKind::VOID:
                    return 0;
            }
            break;
        }

        case TypeKind::ARRAY:
            return arrayElement_->sizeBytes() * static_cast<int>(arrayLength_);

        case TypeKind::UNKNOWN:
            std::unreachable();
    }
    std::unreachable();
}

std::string Type::to_string() const {
    std::string result;
    switch (kind_) {
        case TypeKind::PRIMITIVE: {
            switch (primitive_) {
                case PrimitiveKind::INT:
                    result += "int";
                    break;
                case PrimitiveKind::INT8:
                    result += "int8";
                    break;
                case PrimitiveKind::INT16:
                    result += "int16";
                    break;
                case PrimitiveKind::INT32:
                    result += "int32";
                    break;
                case PrimitiveKind::INT64:
                    result += "int64";
                    break;
                case PrimitiveKind::BOOL:
                    result += "bool";
                    break;
                case PrimitiveKind::VOID:
                    result += "void";
                    break;
            }
            break;
        }

        case TypeKind::ARRAY:
            result +=
                "array[" + arrayElement_->to_string() + " * " + std::to_string(arrayLength_) + "]";
            break;

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

size_t Type::arrayLength() const {
    if (kind_ == TypeKind::ARRAY) {
        return arrayLength_;
    }
    std::unreachable();
}

bool Type::mergeWith(const Type& other) {
    if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) std::unreachable();

    if (!matches(other)) return false;

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

bool Type::matches(const Type& other) const {
    if (family_->isInFamily(other.primitive_) || other.family_->isInFamily(primitive_)) {
        return true;
    }

    if (kind_ != other.kind_) return false;

    switch (kind_) {
        case TypeKind::PRIMITIVE:
            // If one of the types has no family, it is fully determined, so we just need to make
            // sure the other type is compatible
            if (family_->kind() == PrimitiveTypeFamily::Kind::NONE &&
                other.family_->kind() == PrimitiveTypeFamily::Kind::NONE) {
                return primitive_ == other.primitive_;
            }
            if (family_->kind() == PrimitiveTypeFamily::Kind::NONE) {
                return other.family_->isInFamily(primitive_);
            }
            if (other.family_->kind() == PrimitiveTypeFamily::Kind::NONE) {
                return family_->isInFamily(other.primitive_);
            }

            // Both types have families, we must make sure one of the families includes the
            // other type's primitive.
            return family_->isInFamily(other.primitive_) || other.family_->isInFamily(primitive_);

        case TypeKind::ARRAY:
            return arrayLength_ == other.arrayLength_ &&
                   arrayElement_->matches(*other.arrayElement_);

        case TypeKind::UNKNOWN:
            return true;
    }

    std::unreachable();
}
