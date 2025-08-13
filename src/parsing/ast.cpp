#include "parsing/ast.hpp"

#include <cassert>
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
        {TokenKind::EQUAL, Operator::ASSIGN},
        {TokenKind::PLUS_EQUAL, Operator::ADD_ASSIGN},
        {TokenKind::MINUS_EQUAL, Operator::SUBTRACT_ASSIGN},
        {TokenKind::STAR_EQUAL, Operator::MULTIPLY_ASSIGN},
        {TokenKind::SLASH_EQUAL, Operator::DIVIDE_ASSIGN},
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
        {Operator::ASSIGN, "="},
        {Operator::ADD_ASSIGN, "+="},
        {Operator::SUBTRACT_ASSIGN, "-="},
        {Operator::MULTIPLY_ASSIGN, "*="},
        {Operator::DIVIDE_ASSIGN, "/="},
        {Operator::EQUALS, "=="},
        {Operator::NOT_EQUALS, "!="},
        {Operator::LESS_THAN, "<"},
        {Operator::LESS_THAN_OR_EQUAL, "<="},
        {Operator::GREATER_THAN, ">"},
        {Operator::GREATER_THAN_OR_EQUAL, ">="},
    };

    auto it = table.find(op);
    assert(it != table.end() && "Invalid operator");
    return it->second;
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

bool is_assignment_operator(const Operator op) {
    return op == Operator::ASSIGN || op == Operator::ADD_ASSIGN ||
           op == Operator::SUBTRACT_ASSIGN || op == Operator::MULTIPLY_ASSIGN ||
           op == Operator::DIVIDE_ASSIGN;
}

std::string node_kind_to_string(const NodeKind kind) {
    const auto enumName = magic_enum::enum_name(kind);
    return std::string{enumName};
}

}  // namespace AST