#pragma once

#include <string_view>
#include <unordered_map>

#include "type/TypeID.hpp"

namespace CodeGen {

struct SymbolInfo {
    const std::string_view name_;
    const int stackOffset_;
    const int stackSizeBits_;
    const TypeID typeID_;
};

using SymbolTable = std::unordered_map<std::string_view, SymbolInfo>;

}  // namespace CodeGen