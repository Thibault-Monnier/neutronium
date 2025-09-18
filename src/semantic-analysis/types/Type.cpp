#include "semantic-analysis/types/Type.hpp"

const Type& Type::integerFamilyType() {
    static const Type instance{PrimitiveKind::INT, true};
    return instance;
}

const Type& Type::anyFamilyType() {
    static const Type instance{PrimitiveKind::VOID, &AnyTypeFamily::getInstance()};
    return instance;
}

bool Type::matches(const Type& other) const {
    if (family_->isInFamily(other.primitive_) || other.family_->isInFamily(primitive_)) return true;

    if (kind_ == TypeKind::PRIMITIVE && other.kind_ == TypeKind::PRIMITIVE) {
        return primitive_ == other.primitive_;
    }
    if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) {
        return arrayLength_ == other.arrayLength_ && arrayElement_->matches(*other.arrayElement_);
    }

    return false;
}

Type Type::resolve(const Type& other) const {
    if (family_ == &NoTypeFamily::getInstance()) return *this;
    if (other.family_ == &NoTypeFamily::getInstance()) return other;

    if (kind_ == TypeKind::PRIMITIVE && other.kind_ == TypeKind::PRIMITIVE) {
        if (family_->isInFamily(other.primitive_)) return other;
        if (other.family_->isInFamily(primitive_)) return *this;
        std::unreachable();
    }

    if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) {
        return Type{arrayElement_->resolve(*other.arrayElement_), arrayLength_};
    }

    std::unreachable();
}

int Type::sizeBytes() const {
    if (kind_ == TypeKind::PRIMITIVE) {
        switch (primitive_) {
            case PrimitiveKind::INT:
            case PrimitiveKind::INT8:
            case PrimitiveKind::BOOL:
                return 1;
            case PrimitiveKind::INT16:
                return 2;
            case PrimitiveKind::INT32:
                return 4;
            case PrimitiveKind::INT64:
                return 8;
            case PrimitiveKind::VOID:
                return 0;
        }
        std::unreachable();
    } else if (kind_ == TypeKind::ARRAY) {
        return arrayElement_->sizeBytes() * static_cast<int>(arrayLength_);
    }
    std::unreachable();
}

std::string Type::to_string() const {
    if (kind_ == TypeKind::PRIMITIVE) {
        switch (primitive_) {
            case PrimitiveKind::INT:
                return "int";
            case PrimitiveKind::INT8:
                return "int8";
            case PrimitiveKind::INT16:
                return "int16";
            case PrimitiveKind::INT32:
                return "int32";
            case PrimitiveKind::INT64:
                return "int64";
            case PrimitiveKind::BOOL:
                return "bool";
            case PrimitiveKind::VOID:
                return "void";
        }
        std::unreachable();
    } else if (kind_ == TypeKind::ARRAY) {
        return "array[" + arrayElement_->to_string() + " * " + std::to_string(arrayLength_) + "]";
    }
    std::unreachable();
}
