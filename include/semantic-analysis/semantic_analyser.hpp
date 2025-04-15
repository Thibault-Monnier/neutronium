#pragma once

#include <cstdint>
#include <stdexcept>
#include <unordered_map>

#include "parsing/AST.hpp"
#include "utils/log.hpp"

enum class Type : uint8_t {
    INTEGER,
    BOOLEAN,
};

std::string type_to_string(const Type type) {
    switch (type) {
        case Type::INTEGER:
            return "integer";
        case Type::BOOLEAN:
            return "boolean";
        default:
            throw std::invalid_argument("Unknown type passed to type_to_string");
    }
}

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast) : AST_(&ast) {}

    void analyse() {
        for (const auto& stmt : AST_->statements_) {
            analyse_statement(*stmt);
        }

        std::cout << "\033[1;32mAnalysis completed successfully.\033[0m\n";

        for (const auto& [name, type] : variablesTable_) {
            std::cout << "Variable: " << name << ", Type: " << type_to_string(type) << '\n';
        }
    }

   private:
    const AST::Program* AST_;
    std::unordered_map<std::string, Type> variablesTable_;

    [[noreturn]] void abort(const std::string& errorMessage, const std::string& hintMessage = "") {
        print_error(errorMessage);
        if (!hintMessage.empty()) {
            print_hint(hintMessage);
        }
        exit(EXIT_FAILURE);
    }

    bool is_arithmetic_operator(const AST::Operator op) {
        return op == AST::Operator::ADD || op == AST::Operator::SUBTRACT ||
               op == AST::Operator::MULTIPLY || op == AST::Operator::DIVIDE;
    }

    bool is_comparison_operator(const AST::Operator op) {
        return op == AST::Operator::EQUALS || op == AST::Operator::NOT_EQUALS ||
               op == AST::Operator::LESS_THAN || op == AST::Operator::LESS_THAN_OR_EQUAL ||
               op == AST::Operator::GREATER_THAN || op == AST::Operator::GREATER_THAN_OR_EQUAL;
    }

    Type get_variable_type(const std::string& name) {
        const auto it = variablesTable_.find(name);
        if (it == variablesTable_.end()) {
            abort(std::format("Use of undeclared variable: `{}`", name));
        }
        return it->second;
    }

    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr) {
        const Type operandType = get_expression_type(*unaryExpr.operand_);
        if (operandType != Type::INTEGER) {
            abort("Invalid type for unary operation: expected integer, got " +
                  type_to_string(operandType));
        }
        return Type::INTEGER;
    }

    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr) {
        const Type leftType = get_expression_type(*binaryExpr.left_);
        const Type rightType = get_expression_type(*binaryExpr.right_);
        if (leftType != rightType) {
            abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                              type_to_string(leftType), type_to_string(rightType)));
        }

        const AST::Operator op = binaryExpr.operator_;
        if (is_arithmetic_operator(op)) {
            if (leftType != Type::INTEGER) {
                abort("Invalid type for arithmetic operation: expected integer, got " +
                      type_to_string(leftType));
            }
            return Type::INTEGER;
        }

        if (is_comparison_operator(op)) {
            if (leftType != Type::INTEGER) {
                abort("Invalid type for comparison operation: expected integer, got " +
                      type_to_string(leftType));
            }
            return Type::BOOLEAN;
        }

        throw std::invalid_argument("Invalid operator in binary expression");
    }

    Type get_expression_type(const AST::Expression& expr) {
        switch (expr.kind_) {
            case AST::NodeKind::NUMBER_LITERAL:
                return Type::INTEGER;
            case AST::NodeKind::IDENTIFIER: {
                const auto& identifier = static_cast<const AST::Identifier&>(expr);
                return get_variable_type(identifier.name_);
            }
            case AST::NodeKind::UNARY_EXPRESSION: {
                const auto& unaryExpr = static_cast<const AST::UnaryExpression&>(expr);
                return get_unary_expression_type(unaryExpr);
            }
            case AST::NodeKind::BINARY_EXPRESSION: {
                const auto& binaryExpr = static_cast<const AST::BinaryExpression&>(expr);
                return get_binary_expression_type(binaryExpr);
            }
            default:
                throw std::invalid_argument("Invalid expression kind");
        }
    }

    void analyse_declaration_assignment(const AST::Assignment& assignment) {
        const std::string& name = assignment.identifier_->name_;
        if (variablesTable_.contains(name)) {
            abort("Redeclaration of variable: " + name);
        }

        const Type variableType = get_expression_type(*assignment.value_);
        if (variableType != Type::INTEGER) {
            const std::string errorMessage =
                std::format("Invalid variable type: `{}` is declared as {}", name,
                            type_to_string(variableType));
            const std::string hintMessage =
                std::format("Only integer type is allowed for variables");
            abort(errorMessage, hintMessage);
        }

        variablesTable_.emplace(name, variableType);
    }

    void analyse_reassignment(const AST::Assignment& assignment) {
        const std::string& name = assignment.identifier_->name_;

        const auto it = variablesTable_.find(name);
        if (it == variablesTable_.end()) {
            abort("Assignment to undeclared variable: " + name);
        }

        const Type variableType = get_expression_type(*assignment.value_);
        if (variableType != it->second) {
            abort(
                std::format("Type mismatch in assignment: variable `{}` is declared as {}, but "
                            "reassigned to {}",
                            name, type_to_string(it->second), type_to_string(variableType)));
        }
    }

    void analyse_assignment(const AST::Assignment& assignment) {
        if (assignment.isDeclaration_) {
            analyse_declaration_assignment(assignment);
        } else {
            analyse_reassignment(assignment);
        }
    }

    void analyse_exit(const AST::Exit& exitStmt) {
        const Type exitType = get_expression_type(*exitStmt.exitCode_);
        if (exitType != Type::INTEGER) {
            abort("Invalid type for exit statement: expected integer, got " +
                  type_to_string(exitType));
        }
    }

    void analyse_statement(const AST::Statement& stmt) {
        switch (stmt.kind_) {
            case AST::NodeKind::ASSIGNMENT: {
                const auto& assignment = static_cast<const AST::Assignment&>(stmt);
                analyse_assignment(assignment);
                break;
            }
            case AST::NodeKind::EXIT: {
                const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
                analyse_exit(exitStmt);
                break;
            }
            default:
                throw std::invalid_argument("Invalid statement kind");
        }
    }
};