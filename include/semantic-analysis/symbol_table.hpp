#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>

#include "semantic-analysis/type.hpp"

enum class SymbolKind : uint8_t { FUNCTION, VARIABLE };

inline std::string symbol_kind_to_string(const SymbolKind kind) {
    switch (kind) {
        case SymbolKind::FUNCTION:
            return "function";
        case SymbolKind::VARIABLE:
            return "variable";
        default:
            throw std::invalid_argument("Invalid symbol kind passed to symbol_kind_to_string");
    }
}

struct SymbolInfo {
    SymbolKind kind_;
    Type type_;
    std::optional<int> stackOffset_;
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;