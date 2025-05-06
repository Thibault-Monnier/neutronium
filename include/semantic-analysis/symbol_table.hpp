#pragma once

#include <cstdint>
#include <optional>
#include <parsing/ast.hpp>
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
    const SymbolKind kind_;
    const Type type_;
    const AST::Node* declarationNode_;
    const std::optional<int> stackOffset_;
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;