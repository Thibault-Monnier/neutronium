#pragma once

#include <string>

enum class RawType : uint8_t {
    INTEGER,
    BOOLEAN,
    VOID,
    ANY,
};

class Type {
   public:
    // Implicit constructor
    Type(const RawType t) : rawType_(t) {}

    [[nodiscard]] bool matches(const Type& other) const {
        return rawType_ == RawType::ANY || other.rawType_ == RawType::ANY ||
               rawType_ == other.rawType_;
    }

    [[nodiscard]] RawType raw() const { return rawType_; }

    [[nodiscard]] std::string to_string() const {
        switch (rawType_) {
            case RawType::INTEGER:
                return "integer";
            case RawType::BOOLEAN:
                return "boolean";
            case RawType::VOID:
                return "void";
            case RawType::ANY:
                return "any";
            default:
                throw std::invalid_argument("Invalid type passed to TypeInfo::to_string");
        }
    }

   private:
    RawType rawType_;
};
