#pragma once

#include <parsing/ast.hpp>
#include <string>
#include <unordered_map>

#include "types/type.hpp"

enum class SymbolKind : uint8_t { CONSTANT, FUNCTION, VARIABLE };

struct SymbolInfo {
    const std::string name_;
    const SymbolKind kind_;
    const bool isMutable_;
    const Type type_;
    const std::vector<SymbolInfo> parameters_;  // Only for functions
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;
