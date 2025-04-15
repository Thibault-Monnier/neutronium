#pragma once

#include <format>
#include <stdexcept>
#include <string>
#include <utility>

#include "parsing/AST.hpp"
#include "unordered_map"

class Generator {
   public:
    explicit Generator(const AST::Program& ast) : program_(&ast) {}

    std::stringstream generate() {
        // Write the header
        output_ << "section .text\n";
        output_ << "global _start\n";
        output_ << "_start:\n";

        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";

        for (const auto& stmt : program_->statements_) generate_stmt(*stmt.get());

        std::cout << "\033[1;32mGeneration completed successfully.\033[0m\n";

        return std::move(output_);
    }

   private:
    const AST::Program* program_;
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

        variablesStackOffset_.emplace(name, currentStackOffset_);
        return currentStackOffset_;
    }

    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
        output_ << "    mov rax, " << numberLit.value_ << "\n";
    }

    void move_identifier_to_rax(const AST::Identifier& identifier) {
        const int stackOffset = variablesStackOffset_.at(identifier.name_);
        output_ << "    mov rax, [rbp - " << stackOffset << "]\n";
    }

    void evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
        evaluate_expression_to_rax(*unaryExpr.operand_);
        if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
            output_ << "    neg rax\n";
        }
    }

    void evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr) {
        // First evaluate right side to prevent an additional move in case of division, because the
        // numerator has to be in rax
        evaluate_expression_to_rax(*binaryExpr.right_);
        const std::string rhsVariableName =
            std::format(".temp{}", tempVariablesCount_++);  // Starts with '.' to prevent conflicts
        const int rhsVariableStackOffset = stack_allocate_variable(rhsVariableName, "rax");

        evaluate_expression_to_rax(*binaryExpr.left_);

        const AST::Operator op = binaryExpr.operator_;

        if (AST::is_arithmetic_operator(op)) {
            if (op == AST::Operator::DIVIDE) {
                output_ << "    mov rbx, [rbp - " << rhsVariableStackOffset << "]\n";
                output_ << "    cqo\n";
                output_ << "    idiv rbx\n";
            } else {
                const std::unordered_map<AST::Operator, std::string> arithmeticOperatorPrefixes = {
                    {AST::Operator::ADD, "add"},
                    {AST::Operator::SUBTRACT, "sub"},
                    {AST::Operator::MULTIPLY, "imul"},
                };

                output_ << "    " << arithmeticOperatorPrefixes.at(op) << " rax, [rbp - "
                        << rhsVariableStackOffset << "]\n";
            }

        } else if (AST::is_comparison_operator(op)) {
            const std::unordered_map<AST::Operator, std::string> comparisonSuffixes = {
                {AST::Operator::EQUALS, "e"},       {AST::Operator::NOT_EQUALS, "ne"},
                {AST::Operator::LESS_THAN, "l"},    {AST::Operator::LESS_THAN_OR_EQUAL, "le"},
                {AST::Operator::GREATER_THAN, "g"}, {AST::Operator::GREATER_THAN_OR_EQUAL, "ge"},
            };

            output_ << "    cmp rax, [rbp - " << rhsVariableStackOffset << "]\n";
            output_ << "    set" << comparisonSuffixes.at(op) << " al\n";
            output_ << "    movzx rax, al\n";
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
                throw std::invalid_argument("Invalid expression kind");
        }
    }

    void generate_exit(const AST::Exit& exitStmt) {
        evaluate_expression_to_rax(*exitStmt.exitCode_);
        output_ << "    mov rdi, rax\n";  // exit code
        output_ << "    mov rax, 60\n";   // syscall: exit
        output_ << "    syscall\n";
    }

    void generate_assignment(const AST::Assignment& assignmentStmt) {
        const std::string& varName = assignmentStmt.identifier_->name_;

        evaluate_expression_to_rax(*assignmentStmt.value_);
        if (assignmentStmt.isDeclaration_) {
            stack_allocate_variable(varName, "rax");
        } else {
            const int stackOffset = variablesStackOffset_.at(varName);
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
                throw std::invalid_argument("Invalid statement kind");
        }
    }
};