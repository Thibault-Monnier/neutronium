#include "parsing/ast.hpp"

#include <iostream>
#include <magic_enum.hpp>
#include <string>
#include <unordered_map>

#include "lexing/token_kind.hpp"

namespace AST {

Operator token_kind_to_operator(const TokenKind tokenKind) {
    const std::unordered_map<TokenKind, Operator> tokenToOperatorMap = {
        {TokenKind::PLUS, Operator::ADD},
        {TokenKind::MINUS, Operator::SUBTRACT},
        {TokenKind::STAR, Operator::MULTIPLY},
        {TokenKind::SLASH, Operator::DIVIDE},
        {TokenKind::BANG, Operator::LOGICAL_NOT},
        {TokenKind::EQUAL_EQUAL, Operator::EQUALS},
        {TokenKind::BANG_EQUAL, Operator::NOT_EQUALS},
        {TokenKind::LESS_THAN, Operator::LESS_THAN},
        {TokenKind::LESS_THAN_EQUAL, Operator::LESS_THAN_OR_EQUAL},
        {TokenKind::GREATER_THAN, Operator::GREATER_THAN},
        {TokenKind::GREATER_THAN_EQUAL, Operator::GREATER_THAN_OR_EQUAL},
    };

    auto it = tokenToOperatorMap.find(tokenKind);
    return it != tokenToOperatorMap.end() ? it->second : Operator::UNDEFINED_OPERATOR;
}

std::string operator_to_string(const Operator op) {
    static const std::unordered_map<Operator, std::string> table = {
        {Operator::ADD, "+"},
        {Operator::SUBTRACT, "-"},
        {Operator::MULTIPLY, "*"},
        {Operator::DIVIDE, "/"},
        {Operator::LOGICAL_NOT, "!"},
        {Operator::EQUALS, "=="},
        {Operator::NOT_EQUALS, "!="},
        {Operator::LESS_THAN, "<"},
        {Operator::LESS_THAN_OR_EQUAL, "<="},
        {Operator::GREATER_THAN, ">"},
        {Operator::GREATER_THAN_OR_EQUAL, ">="},
    };

    auto it = table.find(op);
    if (it != table.end()) return it->second;
    throw std::invalid_argument("Invalid operator");
}

bool is_arithmetic_operator(const Operator op) {
    return op == Operator::ADD || op == Operator::SUBTRACT || op == Operator::MULTIPLY ||
           op == Operator::DIVIDE;
}

bool is_equality_operator(const Operator op) {
    return op == Operator::EQUALS || op == Operator::NOT_EQUALS;
}

bool is_relational_operator(const Operator op) {
    return op == Operator::LESS_THAN || op == Operator::LESS_THAN_OR_EQUAL ||
           op == Operator::GREATER_THAN || op == Operator::GREATER_THAN_OR_EQUAL;
}

bool is_comparison_operator(const Operator op) {
    return is_equality_operator(op) || is_relational_operator(op);
}

std::string node_kind_to_string(const NodeKind kind) {
    const auto enumName = magic_enum::enum_name(kind);
    return std::string{enumName};
}

}  // namespace AST