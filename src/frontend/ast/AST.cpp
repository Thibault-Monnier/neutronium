#include "AST.hpp"

#include "Operator.hpp"
#include "frontend/lex/TokenKind.hpp"

namespace AST {

Operator tokenKindToOperator(const TokenKind tokenKind) {
    switch (tokenKind) {
        case TokenKind::PLUS:
            return Operator::ADD;
        case TokenKind::MINUS:
            return Operator::SUBTRACT;
        case TokenKind::STAR:
            return Operator::MULTIPLY;
        case TokenKind::SLASH:
            return Operator::DIVIDE;
        case TokenKind::BANG:
            return Operator::LOGICAL_NOT;
        case TokenKind::EQUAL:
            return Operator::ASSIGN;
        case TokenKind::PLUS_EQUAL:
            return Operator::ADD_ASSIGN;
        case TokenKind::MINUS_EQUAL:
            return Operator::SUBTRACT_ASSIGN;
        case TokenKind::STAR_EQUAL:
            return Operator::MULTIPLY_ASSIGN;
        case TokenKind::SLASH_EQUAL:
            return Operator::DIVIDE_ASSIGN;
        case TokenKind::EQUAL_EQUAL:
            return Operator::EQUALS;
        case TokenKind::BANG_EQUAL:
            return Operator::NOT_EQUALS;
        case TokenKind::LESS_THAN:
            return Operator::LESS_THAN;
        case TokenKind::LESS_THAN_EQUAL:
            return Operator::LESS_THAN_OR_EQUAL;
        case TokenKind::GREATER_THAN:
            return Operator::GREATER_THAN;
        case TokenKind::GREATER_THAN_EQUAL:
            return Operator::GREATER_THAN_OR_EQUAL;
        case TokenKind::AMPER_AMPER:
            return Operator::LOGICAL_AND;
        case TokenKind::PIPE_PIPE:
            return Operator::LOGICAL_OR;
        default:
            return Operator::UNDEFINED_OPERATOR;
    }
}

bool isArithmeticOperator(const Operator op) {
    return op == Operator::ADD || op == Operator::SUBTRACT || op == Operator::MULTIPLY ||
           op == Operator::DIVIDE;
}

bool isLogicalOperator(const Operator op) {
    return op == Operator::LOGICAL_OR || op == Operator::LOGICAL_AND;
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