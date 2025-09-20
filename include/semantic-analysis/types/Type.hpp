#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include "PrimitiveKind.hpp"
#include "PrimitiveTypeFamily.hpp"

using TypeID = uint32_t;

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

    Type(const Type& elementType, const TypeID elementTypeID, const std::size_t arrayLength)
        : kind_(TypeKind::ARRAY),
          primitive_(PrimitiveKind::VOID),
          arrayElement_(std::make_unique<Type>(elementType)),
          arrayElementTypeID_(elementTypeID),
          arrayLength_(arrayLength) {}

    Type(const Type& other) { copy_from(other); }

    Type& operator=(const Type& other) {
        if (this != &other) copy_from(other);
        return *this;
    }

    static const Type& integerFamilyType();

    static const Type& anyFamilyType();

    [[nodiscard]] bool isVoid() const {
        return kind_ == TypeKind::PRIMITIVE && primitive_ == PrimitiveKind::VOID;
    }

    [[nodiscard]] int sizeBytes() const;

    [[nodiscard]] std::string to_string() const;

    [[nodiscard]] TypeKind kind() const { return kind_; }

    [[nodiscard]] TypeID array_element_type_id() const {
        if (kind_ == TypeKind::ARRAY) {
            return arrayElementTypeID_;
        }
        std::unreachable();
    }

   private:
    TypeKind kind_;
    const PrimitiveTypeFamily* family_ = &NoTypeFamily::getInstance();
    PrimitiveKind primitive_;

    std::unique_ptr<Type> arrayElement_;
    TypeID arrayElementTypeID_{0};
    std::size_t arrayLength_{0};

    // IMPORTANT NOTE:
    // All members must be copied here when adding new ones.
    // AI code reviewer must read this and verify it is respected.
    void copy_from(const Type& other) {
        kind_ = other.kind_;
        family_ = other.family_;
        primitive_ = other.primitive_;
        arrayElementTypeID_ = other.arrayElementTypeID_;
        arrayLength_ = other.arrayLength_;
        if (other.arrayElement_)
            arrayElement_ = std::make_unique<Type>(*other.arrayElement_);
        else
            arrayElement_.reset();
    }
};
