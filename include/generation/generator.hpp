#pragma once

#include <format>
#include <stdexcept>
#include <string>
#include <utility>

#include "parsing/AST.hpp"
#include "unordered_map"
#include "utils/log.hpp"

class Generator {
   private:
    AST::Program program_;
    std::stringstream output_;

    int currentStackOffset_ = 0;
    int currentStackSize_ = 0;

    int tempVariablesCount_ = 0;

    std::unordered_map<std::string, int> variablesStackOffset_;

    void allocate_stack(const int bytes) {
        output_ << "    sub rsp, " << bytes << "\n";
        currentStackSize_ += bytes;
    }

    int stack_allocate_variable(const std::string& name, const std::string& source) {
        constexpr int sizeBytes = 8;

        currentStackOffset_ += sizeBytes;
        if (currentStackOffset_ > currentStackSize_) {
            allocate_stack(currentStackOffset_ - currentStackSize_ +
                           56);  // Allocate more for less runtime allocations
        }

        output_ << "    mov qword [rbp - " << currentStackOffset_ << "], " << source << "\n";

        const auto [_, isNewVariable] = variablesStackOffset_.emplace(name, currentStackOffset_);
        if (!isNewVariable) {
            const std::string errorMessage =
                std::format("Redeclaration of the following variable: {}", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        return currentStackOffset_;
    }

    int get_variable_stack_offset(const std::string& name) {
        const auto iterator = variablesStackOffset_.find(name);
        if (iterator == variablesStackOffset_.end()) {
            const std::string errorMessage = std::format("Use of undeclared variable: {}", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        return iterator->second;
    }

    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
        output_ << "    mov rax, " << numberLit.value_ << "\n";
    }

    void move_identifier_to_rax(const AST::Identifier& identifier) {
        const int stackOffset = get_variable_stack_offset(identifier.name_);
        output_ << "    mov rax, [rbp - " << stackOffset << "]\n";
    }

    void evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
        evaluate_expression_to_rax(*unaryExpr.operand_);

        switch (unaryExpr.operator_) {
            case AST::Operator::ADD:
                break;
            case AST::Operator::SUBTRACT:
                output_ << "    neg rax\n";
                break;
            default:
                throw std::invalid_argument("Received unary expression with operator {}" +
                                            AST::operator_to_string(unaryExpr.operator_));
        }
    }

    void evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr) {
        // First evaluate right side to prevent an additional move in case of division, because the
        // numerator has to be in rax
        const std::string rhsVariableName =
            std::format(".temp{}", tempVariablesCount_++);  // Starts with '.' to prevent conflicts
                                                            // with source-code variables
        evaluate_expression_to_rax(*binaryExpr.right_);
        const int rhsVariableStackOffset = stack_allocate_variable(rhsVariableName, "rax");

        evaluate_expression_to_rax(*binaryExpr.left_);

        switch (binaryExpr.operator_) {
            case AST::Operator::ADD:
                output_ << "    add rax, [rbp - " << rhsVariableStackOffset << "]\n";
                break;
            case AST::Operator::SUBTRACT:
                output_ << "    sub rax, [rbp - " << rhsVariableStackOffset << "]\n";
                break;
            case AST::Operator::MULTIPLY:
                output_ << "    imul rax, [rbp - " << rhsVariableStackOffset << "]\n";
                break;
            case AST::Operator::DIVIDE:
                output_ << "    mov rbx, [rbp - " << rhsVariableStackOffset << "]\n";
                output_ << "    cqo\n";
                output_ << "    idiv rbx\n";
                break;
            default:
                throw std::invalid_argument("Invalid operator in binary expression, got " +
                                            AST::operator_to_string(binaryExpr.operator_));
        }
    }

    void evaluate_expression_to_rax(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
        switch (expr.kind_) {
            case AST::NodeKind::NUMBER_LITERAL: {
                const auto& numberLit = static_cast<const AST::NumberLiteral&>(expr);
                move_number_lit_to_rax(numberLit);
                break;
            }
            case AST::NodeKind::IDENTIFIER: {
                const auto& identifier = static_cast<const AST::Identifier&>(expr);
                move_identifier_to_rax(identifier);
                break;
            }
            case AST::NodeKind::UNARY_EXPRESSION: {
                const auto& unaryExpr = static_cast<const AST::UnaryExpression&>(expr);
                evaluate_unary_expression_to_rax(unaryExpr);
                break;
            }
            case AST::NodeKind::BINARY_EXPRESSION: {
                const auto& binaryExpr = static_cast<const AST::BinaryExpression&>(expr);
                evaluate_binary_expression_to_rax(binaryExpr);
                break;
            }
            default:
                throw std::invalid_argument(std::format("Cannot evaluate expression of kind {}",
                                                        AST::node_kind_to_string(expr.kind_)));
        }
    }

    void generate_exit(const AST::Exit& exitStmt) {
        evaluate_expression_to_rax(*exitStmt.exitCode_);
        output_ << "    mov rdi, rax\n";  // exit code
        output_ << "    mov rax, 60\n";   // syscall: exit
        output_ << "    syscall\n";
    }

    void generate_assignment(const AST::Assignment& assignmentStmt) {
        const AST::Expression& value = *assignmentStmt.value_.get();
        evaluate_expression_to_rax(value);
        if (assignmentStmt.isDeclaration_) {
            stack_allocate_variable(assignmentStmt.identifier_, "rax");
        } else {
            const int stackOffset = get_variable_stack_offset(assignmentStmt.identifier_);
            output_ << "    mov [rbp - " << stackOffset << "], rax\n";
        }
    }

    void generate_stmt(const AST::Statement& stmt) {
        switch (stmt.kind_) {
            case AST::NodeKind::EXIT: {
                const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
                generate_exit(exitStmt);
                break;
            }
            case AST::NodeKind::ASSIGNMENT: {
                const auto& assignmentStmt = static_cast<const AST::Assignment&>(stmt);
                generate_assignment(assignmentStmt);
                break;
            }
            default:
                throw std::invalid_argument(std::format("Invalid statement kind, {}",
                                                        AST::node_kind_to_string(stmt.kind_)));
        }
    }

   public:
    explicit Generator(AST::Program ast) : program_(std::move(ast)) {}

    std::stringstream generate() {
        // Write the header
        output_ << "section .text\n";
        output_ << "global _start\n";
        output_ << "_start:\n";

        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";

        for (const auto& stmt : program_.statements_) generate_stmt(*stmt.get());

        return std::move(output_);
    }
};