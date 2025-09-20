#include "semantic-analysis/types/Trait.hpp"

[[nodiscard]] std::optional<Trait> trait_from_operator(const AST::Operator op) {
    switch (op) {
        case AST::Operator::ADD:
            return Trait::ADD;
        case AST::Operator::SUBTRACT:
            return Trait::SUB;
        case AST::Operator::MULTIPLY:
            return Trait::MUL;
        case AST::Operator::DIVIDE:
            return Trait::DIV;
        case AST::Operator::LOGICAL_NOT:
            return Trait::NOT;
        case AST::Operator::EQUALS:
        case AST::Operator::NOT_EQUALS:
            return Trait::EQ;
        case AST::Operator::LESS_THAN:
            return Trait::LT;
        case AST::Operator::LESS_THAN_OR_EQUAL:
            return Trait::LTE;
        case AST::Operator::GREATER_THAN:
            return Trait::GT;
        case AST::Operator::GREATER_THAN_OR_EQUAL:
            return Trait::GTE;
        default:
            return std::nullopt;
    }
}