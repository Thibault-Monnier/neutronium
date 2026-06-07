#pragma once
#include <cstdint>
#include <ostream>
#include <string>

class TypeID {
    uint32_t value_ : 31 = 0;

    /// A bool indicating whether the type is a type variable.
    ///
    /// A type variable is a type that has no known properties. Its only purpose is to be merged
    /// with other types. Therefore, we do not need to store anything for it: we can use the TypeID
    /// itself to represent the type variable to save memory.
    bool isVariable_ : 1 = false;

   public:
    explicit TypeID() = default;
    TypeID(const uint32_t value, const bool isVariable) : value_(value), isVariable_(isVariable) {}

    [[nodiscard]] uint32_t value() const { return value_; }
    [[nodiscard]] bool isVariable() const { return isVariable_; }

    void incrementValue() { ++value_; }

    [[nodiscard]] std::string toString() const {
        return isVariable_ ? "TypeVariable(" + std::to_string(value_) + ")"
                           : "TypeID(" + std::to_string(value_) + ")";
    }

   public:
    bool operator==(const TypeID& other) const {
        return value_ == other.value_ && isVariable_ == other.isVariable_;
    }
    bool operator!=(const TypeID& other) const { return !(*this == other); }
};

inline std::ostream& operator<<(std::ostream& os, const TypeID& typeID) {
    os << typeID.toString();
    return os;
}
