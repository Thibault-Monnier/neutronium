#pragma once

#include <string>
#include <unordered_map>

#include "semantic-analysis/types/TypeID.hpp"

namespace CodeGen {

struct SymbolInfo {
    const std::string name_;
    const int stackOffset_;
    const int stackSizeBits_;
    const TypeID typeID_;
};

using SymbolTable = std::unordered_map<std::string, SymbolInfo>;

}  // namespace CodeGen