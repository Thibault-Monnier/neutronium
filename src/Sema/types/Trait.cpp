#include "Sema/Type/Trait.hpp"

#include <magic_enum/magic_enum.hpp>

[[nodiscard]] std::optional<Trait> trait_from_operator(const AST::Operator op) {
    switch (op) {
        case AST::Operator::ADD:
        case AST::Operator::ADD_ASSIGN:
            return Trait::ADD;
        case AST::Operator::SUBTRACT:
        case AST::Operator::SUBTRACT_ASSIGN:
            return Trait::SUB;
        case AST::Operator::MULTIPLY:
        case AST::Operator::MULTIPLY_ASSIGN:
            return Trait::MUL;
        case AST::Operator::DIVIDE:
        case AST::Operator::DIVIDE_ASSIGN:
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
        case AST::Operator::ASSIGN:
        case AST::Operator::UNDEFINED_OPERATOR:
            return std::nullopt;
    }
    std::unreachable();
}

std::string_view trait_to_string(const Trait trait) { return magic_enum::enum_name(trait); }