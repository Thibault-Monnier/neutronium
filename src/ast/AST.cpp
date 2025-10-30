#include "AST.hpp"

#include <unordered_map>

#include "lex/TokenKind.hpp"

namespace AST {

Operator tokenKindToOperator(const TokenKind tokenKind) {
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

bool isArithmeticOperator(const Operator op) {
    return op == Operator::ADD || op == Operator::SUBTRACT || op == Operator::MULTIPLY ||
           op == Operator::DIVIDE;
}

bool isEqualityOperator(const Operator op) {
    return op == Operator::EQUALS || op == Operator::NOT_EQUALS;
}

bool isRelationalOperator(const Operator op) {
    return op == Operator::LESS_THAN || op == Operator::LESS_THAN_OR_EQUAL ||
           op == Operator::GREATER_THAN || op == Operator::GREATER_THAN_OR_EQUAL;
}

bool isComparisonOperator(const Operator op) {
    return isEqualityOperator(op) || isRelationalOperator(op);
}

bool isAssignmentOperator(const Operator op) {
    return op == Operator::ASSIGN || op == Operator::ADD_ASSIGN ||
           op == Operator::SUBTRACT_ASSIGN || op == Operator::MULTIPLY_ASSIGN ||
           op == Operator::DIVIDE_ASSIGN;
}

}  // namespace AST