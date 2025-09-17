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

    Type(const PrimitiveKind t, const PrimitiveTypeFamily* family)
        : kind_(TypeKind::PRIMITIVE), family_(family), primitive_(t) {
        assert(family_->isInFamily(t) && "Type must be in its own family");
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

    static const Type& integerFamilyType() {
        static const Type instance{PrimitiveKind::INT, true};
        return instance;
    }

    static const Type& anyFamilyType() {
        static const Type instance{PrimitiveKind::VOID, &AnyTypeFamily::getInstance()};
        return instance;
    }

    [[nodiscard]] bool matches(const Type& other) const {
        if (family_->isInFamily(other.primitive_) || other.family_->isInFamily(primitive_))
            return true;

        if (kind_ == TypeKind::PRIMITIVE && other.kind_ == TypeKind::PRIMITIVE) {
            return primitive_ == other.primitive_;
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
     * Resolves the current type against another provided type that matches.
     *
     * If one of the types is definitely known (not in a family), that type is chosen.
     * If both types are in families, the most specific type is chosen.
     *
     * @param other The type to resolve against the current type. This type \b must match the current type, or it is undefined behavior.
     * @return The resolved type.
     */
    [[nodiscard]] Type resolve(const Type& other) const {
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
