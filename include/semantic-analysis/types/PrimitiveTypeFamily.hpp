#pragma once

#include "PrimitiveType.hpp"

class PrimitiveTypeFamily {
   public:
    virtual ~PrimitiveTypeFamily() = default;

    enum class Family : uint8_t {
        NONE,
        INTEGER,
    };

    [[nodiscard]] virtual bool isInFamily(PrimitiveType) const = 0;

    static const PrimitiveTypeFamily* familyForType(PrimitiveType t);
};

class IntegerTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const IntegerTypeFamily& getInstance() {
        static const IntegerTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveType t) const override {
        return t == PrimitiveType::INT || t == PrimitiveType::INT8 || t == PrimitiveType::INT16 ||
               t == PrimitiveType::INT32 || t == PrimitiveType::INT64;
    }
};

class NoTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const NoTypeFamily& getInstance() {
        static const NoTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveType) const override { return false; }
};
