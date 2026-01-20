#pragma once

#include <cstdint>
#include <string_view>
#include <unordered_map>

#include "type/TypeID.hpp"

namespace CodeGen {

struct SymbolInfo {
    const std::string_view name_;
    const uint32_t stackOffset_;
    const uint32_t stackSizeBits_;
    const TypeID typeID_;
};

using SymbolTable = std::unordered_map<std::string_view, SymbolInfo>;

}  // namespace CodeGen