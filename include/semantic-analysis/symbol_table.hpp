#pragma once

#include <string>
#include <unordered_map>

#include "semantic-analysis/types.hpp"

struct SymbolInfo {
    Type type_;
    int stackOffset_;
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;