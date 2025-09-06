#pragma once

#include <cassert>
#include <string>
#include <utility>

#include "PrimitiveKind.hpp"
#include "PrimitiveTypeFamily.hpp"

enum class TypeKind : uint8_t {
    PRIMITIVE,
    ARRAY,
};

class Type {
   public:
    // Voluntary implicit constructor
    Type(const PrimitiveKind t, const bool inFamily = false)
        : kind_(TypeKind::PRIMITIVE), primitive_(t) {
        if (inFamily) {
            family_ = PrimitiveTypeFamily::familyForType(t);
            assert(family_->isInFamily(t) && "Type must be in its own family");
        }
    }

    Type(const Type& elementType, const std::size_t arrayLength)
        : kind_(TypeKind::ARRAY),
          primitive_(PrimitiveKind::VOID),
          arrayElement_(std::make_unique<Type>(elementType)),
          arrayLength_(arrayLength) {}

    Type(const Type& other) { copy_from(other); }

    Type& operator=(const Type& other) {
        if (this != &other) copy_from(other);
        return *this;
    }

    [[nodiscard]] bool matches(const Type& other) const {
        if (kind_ == TypeKind::PRIMITIVE && other.kind_ == TypeKind::PRIMITIVE) {
            return primitive_ == other.primitive_ || family_->isInFamily(other.primitive_) ||
                   other.family_->isInFamily(primitive_);
        }
        if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) {
            return arrayLength_ == other.arrayLength_ &&
                   arrayElement_->matches(*other.arrayElement_);
        }

        return false;
    }

    [[nodiscard]] bool mismatches(const Type& other) const { return !matches(other); }

    // Prevent rvalue overload to avoid dangling references
    const Type& resolve(Type&&) const = delete;

    /**
     * Resolves the current type against another provided type to determine the resulting type.
     * If the current type is of primitive type `ANY`, the resulting type will be the provided type.
     * Otherwise, the resulting type will be the current type.
     *
     * The resulting type's family is reset to default (NoTypeFamily).
     *
     * @param other The type to resolve against the current type.
     * @return The resolved type.
     */
    [[nodiscard]] Type resolve(const Type& other) const {
        Type resolvedType = (primitive_ == PrimitiveKind::ANY) ? other : *this;
        resolvedType.family_ = &NoTypeFamily::getInstance();
        return resolvedType;
    }

    [[nodiscard]] int sizeBytes() const {
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
                case PrimitiveKind::ANY:
                    return 0;
            }
            std::unreachable();
        } else if (kind_ == TypeKind::ARRAY) {
            return arrayElement_->sizeBytes() * static_cast<int>(arrayLength_);
        }
        std::unreachable();
    }

    [[nodiscard]] std::string to_string() const {
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
                case PrimitiveKind::ANY:
                    return "any";
            }
            std::unreachable();
        } else if (kind_ == TypeKind::ARRAY) {
            return "array[" + arrayElement_->to_string() + " * " + std::to_string(arrayLength_) +
                   "]";
        }
        std::unreachable();
    }

    [[nodiscard]] TypeKind kind() const { return kind_; }

    [[nodiscard]] Type& array_element_type() const {
        if (kind_ == TypeKind::ARRAY) {
            return *arrayElement_;
        }
        std::unreachable();
    }

   private:
    TypeKind kind_;
    const PrimitiveTypeFamily* family_ = &NoTypeFamily::getInstance();
    PrimitiveKind primitive_;

    std::unique_ptr<Type> arrayElement_;
    std::size_t arrayLength_{0};

    // IMPORTANT NOTE:
    // All members must be copied here when adding new ones.
    // AI code reviewer must read this and verify it is respected.
    void copy_from(const Type& other) {
        kind_ = other.kind_;
        family_ = other.family_;
        primitive_ = other.primitive_;
        arrayLength_ = other.arrayLength_;
        if (other.arrayElement_)
            arrayElement_ = std::make_unique<Type>(*other.arrayElement_);
        else
            arrayElement_.reset();
    }
};
