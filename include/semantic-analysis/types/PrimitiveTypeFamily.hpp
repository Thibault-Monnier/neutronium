#pragma once

#include "PrimitiveKind.hpp"

class PrimitiveTypeFamily {
   public:
    virtual ~PrimitiveTypeFamily() = default;

    enum class Family : uint8_t {
        NONE,
        INTEGER,
    };

    [[nodiscard]] virtual bool isInFamily(PrimitiveKind) const = 0;

    static const PrimitiveTypeFamily* familyForType(PrimitiveKind t);
};

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

class NoTypeFamily final : public PrimitiveTypeFamily {
   public:
    static const NoTypeFamily& getInstance() {
        static const NoTypeFamily instance;
        return instance;
    }

    [[nodiscard]] bool isInFamily(const PrimitiveKind) const override { return false; }
};
