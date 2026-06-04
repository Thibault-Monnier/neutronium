#pragma once

#include <cassert>
#include <cstdint>
#include <utility>

namespace IR {

/// Represents a type in the IR.
class Type {
   public:
    enum class Kind : uint8_t { INT, PTR, ARRAY, VOID };

   private:
    const Kind kind_;

    /// Used to store additional data about the type. For example, if this Type is an integer, then
    /// data_ is the size of the integer in bits. If this Type is an array, then data_ is the number
    /// of elements in the array.
    const uint32_t data_ = 0;

    /// Used to represent a subtype of this type. For example, if this Type is a pointer, then
    /// subtype_ is the type of the value it points to. If this Type is an array, then
    /// subtype_ is the type of the elements of the array.
    const Type* const subtype_ = nullptr;

    explicit Type(const Kind kind) : kind_(kind) {}
    explicit Type(const Kind kind, const Type* subtype) : kind_(kind), subtype_(subtype) {
        assert(holdsSubtype());
    }
    explicit Type(const Kind kind, const Type* elementType, const uint32_t elementCount)
        : kind_(kind), data_(elementCount), subtype_(elementType) {
        assert(isArray());
    }
    explicit Type(const Kind kind, const uint8_t integerSizeBits)
        : kind_(kind), data_(integerSizeBits) {
        assert(kind == Kind::INT);
    }

   public:
    [[nodiscard]] Kind getKind() const { return kind_; }
    [[nodiscard]] uint8_t getIntegerSizeBits() const {
        assert(kind_ == Kind::INT);
        return data_;
    }
    [[nodiscard]] uint32_t getArrayElementCount() const {
        assert(isArray());
        return data_;
    }
    [[nodiscard]] const Type& getSubtype() const {
        assert(holdsSubtype());
        return *subtype_;
    }

    /// Calculates the total storage space required for this type, in bits.
    [[nodiscard]] uint32_t computeSizeBits() const {
        switch (kind_) {
            case Kind::PTR:
                return 64;
            case Kind::INT:
                return getIntegerSizeBits();
            case Kind::ARRAY:
                return getArrayElementCount() * subtype_->computeSizeBits();
            case Kind::VOID:
                break;
        }

        std::unreachable();
    }

    /// Calculates the total storage space required for this type, in bytes.
    [[nodiscard]] uint32_t computeSizeBytes() const {
        switch (kind_) {
            case Kind::PTR:
            case Kind::INT:
                return (computeSizeBits() + 7) / 8;  // Align to the nearest byte
            case Kind::ARRAY:
                return getArrayElementCount() * subtype_->computeSizeBytes();
            case Kind::VOID:
                break;
        }

        std::unreachable();
    }

    [[nodiscard]] bool holdsSubtype() const { return kind_ == Kind::PTR || kind_ == Kind::ARRAY; }

    [[nodiscard]] bool isVoid() const { return kind_ == Kind::VOID; }
    [[nodiscard]] bool isInteger() const { return kind_ == Kind::INT; }
    [[nodiscard]] bool isBoolean() const { return isInteger() && data_ == 1; }
    [[nodiscard]] bool isPointer() const { return kind_ == Kind::PTR; }
    [[nodiscard]] bool isArray() const { return kind_ == Kind::ARRAY; }

    /// A scalar type is a type that can be stored in a single register.
    [[nodiscard]] bool isScalar() const { return isInteger() || isPointer(); }

    static Type intType(const uint32_t sizeBits) {
        assert(sizeBits == 1 || sizeBits == 8 || sizeBits == 16 || sizeBits == 32 ||
               sizeBits == 64);
        return Type(Kind::INT, sizeBits);
    }
    static Type boolean() { return intType(1); }
    static Type voidType() { return Type(Kind::VOID); }
    static Type pointer(const Type* pointeeType) { return Type(Kind::PTR, pointeeType); }
    static Type array(const Type* elementType, const uint32_t elementCount) {
        return Type(Kind::ARRAY, elementType, elementCount);
    }

    bool operator==(const Type& other) const {
        if (kind_ != other.kind_) return false;
        if (data_ != other.data_) return false;

        if (!subtype_ && !other.subtype_) return true;
        if (subtype_ && other.subtype_) return *subtype_ == *other.subtype_;

        return false;
    }
};

}  // namespace IR
