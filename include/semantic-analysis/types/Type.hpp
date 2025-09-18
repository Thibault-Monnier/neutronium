#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include "PrimitiveKind.hpp"
#include "PrimitiveTypeFamily.hpp"
#include "TypeInferrer.hpp"

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

    static const Type& integerFamilyType();

    static const Type& anyFamilyType();

    [[nodiscard]] bool matches(const Type& other) const;

    [[nodiscard]] bool mismatches(const Type& other) const { return !matches(other); }

    // Prevent rvalue overload to avoid dangling references
    const Type& resolve(Type&&) const = delete;

    /**
     * Resolves the current type against another provided type that matches.
     *
     * If one of the types is definitely known (not in a family), that type is chosen.
     * If both types are in families, the most specific type is chosen.
     *
     * @param other The type to resolve against the current type. This type \b must match the
     * current type, or it is undefined behavior.
     * @return The resolved type.
     */
    [[nodiscard]] Type resolve(const Type& other) const;

    [[nodiscard]] int sizeBytes() const;

    [[nodiscard]] std::string to_string() const;

    [[nodiscard]] TypeKind kind() const { return kind_; }

    [[nodiscard]] Type& array_element_type() const {
        if (kind_ == TypeKind::ARRAY) {
            return *arrayElement_;
        }
        std::unreachable();
    }

   private:
    TypeInferrer typeInferrer_;

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
