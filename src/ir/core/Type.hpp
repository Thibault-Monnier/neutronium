#pragma once

#include <cassert>
#include <cstdint>

namespace IR {

/// Represents a type in the IR.
class Type {
   public:
    enum class Kind : uint8_t { INT8, INT16, INT32, INT64, BOOL, PTR, VOID };

    explicit Type(const Kind kind) : kind_(kind) {}
    explicit Type(const Kind kind, Type* pointeeType) : kind_(kind), pointeeType_(pointeeType) {}

   private:
    Kind kind_;

    /// Used to represent a subtype of this type. For example, if this Type is a pointer, then
    /// pointeeType_ is the type of the value it points to.
    Type* const pointeeType_ = nullptr;

   public:
    [[nodiscard]] Kind getKind() const { return kind_; }
    [[nodiscard]] Type* getPointeeType() const {
        assert(kind_ == Kind::PTR);
        return pointeeType_;
    }

    static Type int8() { return Type(Kind::INT8); }
    static Type int16() { return Type(Kind::INT16); }
    static Type int32() { return Type(Kind::INT32); }
    static Type int64() { return Type(Kind::INT64); }
    static Type boolean() { return Type(Kind::BOOL); }
    static Type voidType() { return Type(Kind::VOID); }
    static Type pointer(Type* pointeeType) { return Type(Kind::PTR, pointeeType); }

    bool operator==(const Type& other) const {
        return kind_ == other.kind_ && *pointeeType_ == *other.pointeeType_;
    }
};

}  // namespace IR
