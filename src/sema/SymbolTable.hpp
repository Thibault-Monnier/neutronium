#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "type/TypeID.hpp"

enum class SymbolKind : uint8_t { FUNCTION, VARIABLE };

struct SymbolInfo {
    const std::string name_;
    const SymbolKind kind_;
    const bool isMutable_;
    const TypeID typeID_;
    const std::vector<SymbolInfo> parameters_;  // Only for functions
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;
