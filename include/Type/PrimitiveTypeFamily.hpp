#pragma once

#include "Primitive.hpp"

/**
 * @class PrimitiveTypeFamily
 * @brief Abstract base class containing basic operations to handle primitive type families.
 */
class PrimitiveTypeFamily {
   public:
    enum class Kind : uint8_t { INTEGER, ANY, NONE };

    virtual ~PrimitiveTypeFamily() = default;

    [[nodiscard]] virtual bool isInFamily(Primitive::Kind) const = 0;
    [[nodiscard]] virtual Kind kind() const = 0;

    static const PrimitiveTypeFamily* familyForType(Primitive::Kind t);
};

/**
 * @class IntegerTypeFamily
 * @brief Defines the integer type family.
 *
 * This class is a specialization of the PrimitiveTypeFamily that specifically handles
 * Type categorized as integers, such as INT, INT8, INT16, INT32, and INT64.
 */
class IntegerTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const IntegerTypeFamily& getInstance() {
        static const IntegerTypeFamily instance;
        return instance;
    }

    [[nodiscard]] Kind kind() const override { return Kind::INTEGER; }

    [[nodiscard]] bool isInFamily(const Primitive::Kind t) const override {
        return t == Primitive::Kind::INT || t == Primitive::Kind::INT8 ||
               t == Primitive::Kind::INT16 || t == Primitive::Kind::INT32 ||
               t == Primitive::Kind::INT64;
    }
};

/**
 * @class AnyTypeFamily
 * @brief Represents a special primitive type family encompassing all possible primitive kinds.
 *
 * This class is an implementation of the `PrimitiveTypeFamily` interface, providing
 * functionality to represent a type family that includes all Type defined in the system.
 */
class AnyTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const AnyTypeFamily& getInstance() {
        static const AnyTypeFamily instance;
        return instance;
    }

    [[nodiscard]] Kind kind() const override { return Kind::ANY; }

    [[nodiscard]] bool isInFamily(const Primitive::Kind) const override { return true; }
};

class NoTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const NoTypeFamily& getInstance() {
        static const NoTypeFamily instance;
        return instance;
    }

    [[nodiscard]] Kind kind() const override { return Kind::NONE; }

    [[nodiscard]] bool isInFamily(const Primitive::Kind) const override { return false; }
};
