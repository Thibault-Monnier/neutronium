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

    std::unordered_map<std::string, int> variableStackOffset_;

    void allocate_stack(const int bytes) {
        output_ << "    sub rsp, " << bytes << "\n";
        currentStackSize_ += bytes;
    }

    void allocate_variable(const std::string& name, const std::string& asmOperand, const int size) {
        if (currentStackOffset_ + size > currentStackSize_) {
            allocate_stack(size * 8);  // Allocate a little more to do less runtime allocations
        }

        std::string movDirective;
        if (size == 1)
            movDirective = "byte";
        else if (size == 2)
            movDirective = "word";
        else if (size == 4)
            movDirective = "dword";
        else if (size == 8)
            movDirective = "qword";
        else {
            throw std::invalid_argument(std::format("Cannot allocate a {} byte variable", size));
        }

        currentStackOffset_ += size;
        output_ << "    mov " << movDirective << " [rbp - " << currentStackOffset_ << "], "
                << asmOperand << "\n";

        if (!variableStackOffset_.emplace(name, currentStackOffset_).second) {
            const std::string errorMessage =
                std::format("The following variable cannot be redeclared: {}", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }
    }

    int get_variable_stack_offset(const std::string& name) {
        const auto itr = variableStackOffset_.find(name);
        if (itr == variableStackOffset_.end()) {
            const std::string errorMessage = std::format("Use of undeclared variable: {}", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        return itr->second;
    }

    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
        output_ << "    mov rax, " << numberLit.value_ << "\n";
    }

    void move_identifier_to_rax(const AST::Identifier& identifier) {
        const int stackOffset = get_variable_stack_offset(identifier.name_);
        output_ << "    mov rax, [rbp - " << stackOffset << "]\n";
    }

    void evaluate_unary_expression_to_rax(
        const AST::UnaryExpression& unaryExpr) {  // NOLINT(*-no-recursion)
        evaluate_expression_to_rax(*unaryExpr.operand_);
        switch (unaryExpr.operator_) {
            case AST::Operator::ADD:
                break;
            case AST::Operator::SUBTRACT:
                output_ << "    neg rax\n";
                break;
            default:
                throw std::invalid_argument(
                    std::format("Cannot evaluate unary expression with operator {}",
                                AST::operator_to_string(unaryExpr.operator_)));
        }
    }

    void evaluate_binary_expression_to_rax(
        const AST::BinaryExpression& binaryExpr) {  // NOLINT(*-no-recursion)
        evaluate_expression_to_rax(*binaryExpr.right_);
        const std::string name =
            std::format(".temp{}", tempVariablesCount_++);  // Starts with '.' to prevent conflicts
                                                            // with source-code variables
        allocate_variable(name, "rax", 8);

        evaluate_expression_to_rax(*binaryExpr.left_);

        switch (binaryExpr.operator_) {
            case AST::Operator::ADD:
                output_ << "    add rax, [rbp - " << get_variable_stack_offset(name) << "]\n";
                break;
            case AST::Operator::SUBTRACT:
                output_ << "    sub rax, [rbp - " << get_variable_stack_offset(name) << "]\n";
                break;
            case AST::Operator::MULTIPLY:
                output_ << "    imul rax, [rbp - " << get_variable_stack_offset(name) << "]\n";
                break;
            case AST::Operator::DIVIDE:
                output_ << "    mov rbx, [rbp - " << get_variable_stack_offset(name) << "]\n";
                output_ << "    cqo\n";
                output_ << "    idiv rbx\n";  // The left-side is already in rax
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
        const AST::Expression& expr = *assignmentStmt.value_.get();
        evaluate_expression_to_rax(expr);
        allocate_variable(assignmentStmt.identifier_, "rax", 8);
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

        for (const auto& stmt : program_.statements_) {
            if (stmt->kind_ == AST::NodeKind::EXIT) {
                const auto exitStmt = static_cast<AST::Exit*>(stmt.get());
                generate_exit(*exitStmt);
            } else if (stmt->kind_ == AST::NodeKind::ASSIGNMENT) {
                const auto assignmentStmt = static_cast<AST::Assignment*>(stmt.get());
                generate_assignment(*assignmentStmt);
            } else {
                print_error("Unknown statement kind");
                exit(EXIT_FAILURE);
            }
        }

        return std::move(output_);
    }
};