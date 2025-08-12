#pragma once

#include <string>
#include <utility>

enum class PrimitiveType : uint8_t {
    INTEGER,
    BOOLEAN,
    VOID,
    ANY,
};

enum class TypeKind : uint8_t {
    PRIMITIVE,
    ARRAY,
};

class Type {
   public:
    // Implicit constructor
    Type(const PrimitiveType t) : primitive_(t) {}

    Type(const Type& elementType, const std::size_t arrayLength)
        : kind_(TypeKind::ARRAY),
          arrayElement_(std::make_unique<Type>(elementType)),
          arrayLength_(arrayLength) {}

    Type(const Type& other) { copy_from(other); }

    Type& operator=(const Type& other) {
        if (this != &other) copy_from(other);
        return *this;
    }

    [[nodiscard]] bool matches(const Type& other, const bool looseMatch = false) const {
        if (looseMatch) {
            // If loose match, allow ANY type to match with any type
            if (primitive_ == PrimitiveType::ANY || other.primitive_ == PrimitiveType::ANY)
                return true;
        }

        if (kind_ == TypeKind::PRIMITIVE && other.kind_ == TypeKind::PRIMITIVE) {
            return primitive_ == other.primitive_;
        }
        if (kind_ == TypeKind::ARRAY && other.kind_ == TypeKind::ARRAY) {
            return arrayLength_ == other.arrayLength_ &&
                   arrayElement_->matches(*other.arrayElement_);
        }

        return false;
    }

    [[nodiscard]] bool mismatches(const Type& other, const bool looseMatch = false) const {
        return !matches(other, looseMatch);
    }

    [[nodiscard]] std::string to_string() const {
        if (kind_ == TypeKind::PRIMITIVE) {
            switch (primitive_) {
                case PrimitiveType::INTEGER:
                    return "integer";
                case PrimitiveType::BOOLEAN:
                    return "boolean";
                case PrimitiveType::VOID:
                    return "void";
                case PrimitiveType::ANY:
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

    [[nodiscard]] Type array_element_type() const {
        if (kind_ == TypeKind::ARRAY) {
            return *arrayElement_;
        }
        std::unreachable();
    }

   private:
    TypeKind kind_{TypeKind::PRIMITIVE};

    PrimitiveType primitive_{PrimitiveType::VOID};

    std::unique_ptr<Type> arrayElement_;
    std::size_t arrayLength_{0};

    void copy_from(const Type& other) {
        kind_ = other.kind_;
        primitive_ = other.primitive_;
        arrayLength_ = other.arrayLength_;
        if (other.arrayElement_)
            arrayElement_ = std::make_unique<Type>(*other.arrayElement_);
        else
            arrayElement_.reset();
    }
};
