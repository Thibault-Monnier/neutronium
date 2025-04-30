#pragma once

#include <ranges>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "parsing/AST.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "semantic-analysis/types.hpp"
#include "utils/log.hpp"

struct Scope {
    std::unordered_map<std::string, int> variablesStackOffset_;
    int frameSize_;
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
    explicit SemanticAnalyser(const AST::Program& ast) : ast_(&ast) {}

    SymbolTable analyse() {
        analyse_statement(*ast_->body_);

        for (const auto& [name, info] : symbolTable_) {
            std::cout << "Variable: " << name << ", Type: " << type_to_string(info.type_) << '\n';
        }

        std::cout << "\033[1;32mAnalysis completed successfully.\033[0m\n";

        return symbolTable_;
    }

   private:
    const AST::Program* ast_;

    SymbolTable symbolTable_;

    int currentStackOffset_ = 0;
    std::vector<Scope> scopeVariablesStackOffset_;

    [[noreturn]] void abort(const std::string& errorMessage, const std::string& hintMessage = "") {
        print_error(errorMessage);
        if (!hintMessage.empty()) {
            print_hint(hintMessage);
        }
        exit(EXIT_FAILURE);
    }

    void enter_scope(const AST::BlockStatement& blockStmt) {
        Scope scope;
        int frameSize = 0;
        for (const auto& stmt : blockStmt.body_) {
            if (stmt->kind_ == AST::NodeKind::ASSIGNMENT) {
                const auto& assignment = static_cast<AST::Assignment&>(*stmt);
                if (assignment.isDeclaration_) {
                    frameSize += 8;
                    scope.variablesStackOffset_[assignment.identifier_->name_] =
                        currentStackOffset_ + frameSize;
                }
            }
        }

        scope.frameSize_ = (frameSize + 15) & ~15;
        currentStackOffset_ += scope.frameSize_;
        scopeVariablesStackOffset_.push_back(scope);
    }

    void exit_scope() {
        currentStackOffset_ -= scopeVariablesStackOffset_.back().frameSize_;
        scopeVariablesStackOffset_.pop_back();
    }

    bool is_variable_declared_in_scope(const std::string& name) {
        for (auto& scopeIt : std::ranges::reverse_view(scopeVariablesStackOffset_)) {
            if (scopeIt.variablesStackOffset_.contains(name)) {
                return true;
            }
        }
        return false;
    }

    Type get_scope_variable_type(const std::string& name) { return symbolTable_.at(name).type_; }

    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr) {
        const Type operandType = get_expression_type(*unaryExpr.operand_);
        if (operandType == Type::INTEGER) {
            if (AST::is_arithmetic_operator(unaryExpr.operator_)) return Type::INTEGER;

            abort("Invalid unary operator for integer operand: got " +
                  AST::operator_to_string(unaryExpr.operator_));
        }
        if (operandType == Type::BOOLEAN) {
            if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) return Type::BOOLEAN;

            abort("Invalid unary operator for boolean operand: got " +
                  AST::operator_to_string(unaryExpr.operator_));
        }

        abort("Invalid type for unary operation: expected integer or boolean, got " +
              type_to_string(operandType));
    }

    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr) {
        const Type leftType = get_expression_type(*binaryExpr.left_);
        const Type rightType = get_expression_type(*binaryExpr.right_);
        if (leftType != rightType) {
            abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                              type_to_string(leftType), type_to_string(rightType)));
        }

        const AST::Operator op = binaryExpr.operator_;
        if (AST::is_arithmetic_operator(op)) {
            if (leftType != Type::INTEGER) {
                abort("Invalid type for arithmetic operation: expected integer, got " +
                      type_to_string(leftType));
            }
            return Type::INTEGER;
        }

        if (AST::is_equality_operator(op)) {
            if (leftType != Type::INTEGER && leftType != Type::BOOLEAN) {
                abort("Invalid type for equality operation: expected integer or boolean, got " +
                      type_to_string(leftType));
            }
            return Type::BOOLEAN;
        }

        if (AST::is_relational_operator(op)) {
            if (leftType != Type::INTEGER) {
                abort("Invalid type for relational operation: expected integer, got " +
                      type_to_string(leftType));
            }
            return Type::BOOLEAN;
        }

        throw std::invalid_argument("Invalid operator in binary expression");
    }

    Type get_expression_type(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
        switch (expr.kind_) {
            case AST::NodeKind::NUMBER_LITERAL:
                return Type::INTEGER;
            case AST::NodeKind::BOOLEAN_LITERAL:
                return Type::BOOLEAN;
            case AST::NodeKind::IDENTIFIER: {
                const auto& identifier = static_cast<const AST::Identifier&>(expr);
                if (!is_variable_declared_in_scope(identifier.name_)) {
                    abort(std::format("Attempted to access undeclared variable: `{}`",
                                      identifier.name_));
                }
                return get_scope_variable_type(identifier.name_);
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
        if (symbolTable_.contains(name)) {
            abort(std::format("Redeclaration of variable: `{}`", name),
                  "Shadowing is not permitted, even for disjoint scopes");
        }

        const Type variableType = get_expression_type(*assignment.value_);
        if (variableType != Type::INTEGER && variableType != Type::BOOLEAN) {
            abort(std::format("Invalid variable type: `{}` is declared as {}", name,
                              type_to_string(variableType)));
        }

        const int stackOffset = scopeVariablesStackOffset_.back().variablesStackOffset_.at(name);
        symbolTable_.emplace(name, SymbolInfo{.type_ = variableType, .stackOffset_ = stackOffset});
    }

    void analyse_reassignment(const AST::Assignment& assignment) {
        const std::string& name = assignment.identifier_->name_;

        if (!is_variable_declared_in_scope(name)) {
            abort(std::format("Assignment to undeclared variable: `{}`", name));
        }

        const Type declaredType = get_scope_variable_type(name);
        const Type variableType = get_expression_type(*assignment.value_);
        if (variableType != declaredType) {
            abort(
                std::format("Type mismatch in assignment: variable `{}` is declared as {}, but "
                            "reassigned to {}",
                            name, type_to_string(declaredType), type_to_string(variableType)));
        }
    }

    void analyse_assignment(const AST::Assignment& assignment) {
        if (assignment.isDeclaration_) {
            analyse_declaration_assignment(assignment);
        } else {
            analyse_reassignment(assignment);
        }
    }

    void analyse_expression(const AST::Expression& expr, const Type expected,
                            const std::string& location) {
        const Type type = get_expression_type(expr);
        if (type != expected) {
            abort(std::format("Invalid expression type for {}: expected {}, got {}", location,
                              type_to_string(expected), type_to_string(type)));
        }
    }

    void analyse_if_statement(const AST::IfStatement& ifStmt) {  // NOLINT(*-no-recursion)
        analyse_expression(*ifStmt.condition_, Type::BOOLEAN, "condition");
        analyse_statement(*ifStmt.body_);
    }

    void analyse_while_statement(const AST::WhileStatement& whileStmt) {  // NOLINT(*-no-recursion)
        analyse_expression(*whileStmt.condition_, Type::BOOLEAN, "condition");
        analyse_statement(*whileStmt.body_);
    }

    void analyse_exit(const AST::Exit& exitStmt) {
        analyse_expression(*exitStmt.exitCode_, Type::INTEGER, "exit code");
    }

    void analyse_statement(const AST::Statement& stmt) {  // NOLINT(*-no-recursion)
        switch (stmt.kind_) {
            case AST::NodeKind::ASSIGNMENT: {
                const auto& assignment = static_cast<const AST::Assignment&>(stmt);
                analyse_assignment(assignment);
                break;
            }
            case AST::NodeKind::IF_STATEMENT: {
                const auto& ifStmt = static_cast<const AST::IfStatement&>(stmt);
                analyse_if_statement(ifStmt);
                break;
            }
            case AST::NodeKind::WHILE_STATEMENT: {
                const auto& whileStmt = static_cast<const AST::WhileStatement&>(stmt);
                analyse_while_statement(whileStmt);
                break;
            }
            case AST::NodeKind::EXIT: {
                const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
                analyse_exit(exitStmt);
                break;
            }
            case AST::NodeKind::BLOCK_STATEMENT: {
                const auto& blockStmt = static_cast<const AST::BlockStatement&>(stmt);
                enter_scope(blockStmt);
                for (const auto& innerStmt : blockStmt.body_) {
                    analyse_statement(*innerStmt);
                }
                exit_scope();
                break;
            }
            default:
                throw std::invalid_argument("Invalid statement kind at semantic analysis");
        }
    }
};