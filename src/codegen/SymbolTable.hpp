#pragma once

#include <cstdint>
#include <string_view>
#include <unordered_map>

#include "type/TypeID.hpp"

namespace CodeGen {

struct SymbolInfo {
    const uint32_t stackOffset_;
    const TypeID typeID_;
};

using SymbolTable = std::unordered_map<std::string_view, SymbolInfo>;

}  // namespace CodeGen