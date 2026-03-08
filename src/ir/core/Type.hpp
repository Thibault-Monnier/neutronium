#pragma once

#include <cassert>
#include <cstdint>

namespace IR {

/// Represents a type in the IR.
class Type {
   public:
    enum class Kind : uint8_t { INT, PTR, VOID };

   private:
    const Kind kind_;

    /// Used to represent the size of an integer type in bits. Only applicable if this is an integer
    /// type.
    const uint8_t integerSizeBits_ = 0;

    /// Used to represent a subtype of this type. For example, if this Type is a pointer, then
    /// pointeeType_ is the type of the value it points to.
    const Type* const pointeeType_ = nullptr;

    explicit Type(const Kind kind) : kind_(kind) {}
    explicit Type(const Kind kind, const Type* pointeeType)
        : kind_(kind), pointeeType_(pointeeType) {
        assert(kind == Kind::PTR);
    }
    explicit Type(const Kind kind, const uint8_t integerSizeBits)
        : kind_(kind), integerSizeBits_(integerSizeBits) {
        assert(kind == Kind::INT);
    }

   public:
    [[nodiscard]] Kind getKind() const { return kind_; }
    [[nodiscard]] uint8_t integerSizeBits() const {
        assert(kind_ == Kind::INT);
        return integerSizeBits_;
    }
    [[nodiscard]] const Type& getPointeeType() const {
        assert(kind_ == Kind::PTR);
        return *pointeeType_;
    }

    [[nodiscard]] bool isVoid() const { return kind_ == Kind::VOID; }
    [[nodiscard]] bool isInteger() const { return kind_ == Kind::INT; }
    [[nodiscard]] bool isBoolean() const { return isInteger() && integerSizeBits_ == 1; }
    [[nodiscard]] bool isPointer() const { return kind_ == Kind::PTR; }

    static Type intType(const uint32_t sizeBits) {
        assert(sizeBits == 1 || sizeBits == 8 || sizeBits == 16 || sizeBits == 32 ||
               sizeBits == 64);
        return Type(Kind::INT, sizeBits);
    }
    static Type boolean() { return intType(1); }
    static Type voidType() { return Type(Kind::VOID); }
    static Type pointer(const Type* pointeeType) { return Type(Kind::PTR, pointeeType); }

    bool operator==(const Type& other) const {
        return kind_ == other.kind_ && integerSizeBits_ == other.integerSizeBits_ &&
               pointeeType_ == other.pointeeType_ &&
               (pointeeType_ == nullptr || *pointeeType_ == *other.pointeeType_);
    }
};

}  // namespace IR
