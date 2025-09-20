#pragma once

#include "PrimitiveKind.hpp"

/**
 * @class PrimitiveTypeFamily
 * @brief Abstract base class containing basic operations to handle primitive type families.
 */
class PrimitiveTypeFamily {
   public:
    virtual ~PrimitiveTypeFamily() = default;

    [[nodiscard]] virtual bool isInFamily(PrimitiveKind) const = 0;

    static const PrimitiveTypeFamily* familyForType(PrimitiveKind t);
};

/**
 * @class IntegerTypeFamily
 * @brief Defines the integer type family.
 *
 * This class is a specialization of the PrimitiveTypeFamily that specifically handles
 * types categorized as integers, such as INT, INT8, INT16, INT32, and INT64.
 */
class IntegerTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const IntegerTypeFamily& getInstance() {
        static const IntegerTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveKind t) const override {
        return t == PrimitiveKind::INT || t == PrimitiveKind::INT8 || t == PrimitiveKind::INT16 ||
               t == PrimitiveKind::INT32 || t == PrimitiveKind::INT64;
    }
};

/**
 * @class AnyTypeFamily
 * @brief Represents a special primitive type family encompassing all possible primitive kinds.
 *
 * This class is an implementation of the `PrimitiveTypeFamily` interface, providing
 * functionality to represent a type family that includes all types defined in the system.
 */
class AnyTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const AnyTypeFamily& getInstance() {
        static const AnyTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveKind) const override { return true; }
};

class NoTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const NoTypeFamily& getInstance() {
        static const NoTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveKind) const override { return false; }
};
