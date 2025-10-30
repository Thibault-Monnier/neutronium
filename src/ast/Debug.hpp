#pragma once

#include "AST.hpp"
#include "type/TypeManager.hpp"

namespace AST {

std::string operator_to_string(Operator op);
std::string node_kind_to_string(NodeKind kind);

void log_ast(const Program& programNode);

}  // namespace AST