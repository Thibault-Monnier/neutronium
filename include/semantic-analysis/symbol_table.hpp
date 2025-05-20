#pragma once

#include <optional>
#include <parsing/ast.hpp>
#include <string>
#include <unordered_map>

#include "semantic-analysis/type.hpp"

enum class SymbolKind : uint8_t { CONSTANT, FUNCTION, VARIABLE };

inline std::string symbol_kind_to_string(const SymbolKind kind) {
    switch (kind) {
        case SymbolKind::CONSTANT:
            return "constant";
        case SymbolKind::FUNCTION:
            return "function";
        case SymbolKind::VARIABLE:
            return "variable";
        default:
            throw std::invalid_argument("Invalid symbol kind passed to symbol_kind_to_string");
    }
}

struct SymbolInfo {
    const std::string name_;
    const SymbolKind kind_;
    const bool isMutable_;
    const Type type_;
    const AST::Node* declarationNode_;
    const std::optional<int> stackOffset_;      // Only for variables
    const std::vector<SymbolInfo> parameters_;  // Only for functions
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;
