#pragma once

#include <format>
#include <stdexcept>
#include <string>
#include <utility>

#include "parsing/AST.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "unordered_map"

class Generator {
   public:
    explicit Generator(const AST::Program& ast, SymbolTable symbolTable)
        : program_(&ast), symbolTable_(std::move(symbolTable)) {}

    std::stringstream generate() {
        // Write the header
        output_ << "section .text\n";
        output_ << "global _start\n";
        output_ << "_start:\n";

        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";

        generate_stmt(*program_->body_);

        std::cout << output_.str();
        std::cout << "\033[1;32mGeneration completed successfully.\033[0m\n";

        return std::move(output_);
    }

   private:
    const AST::Program* program_;
    std::stringstream output_;

    int ifStmtsCount_ = 0;

    SymbolTable symbolTable_;

    int get_current_scope_frame_size(const AST::BlockStatement& blockStmt) const {
        int frameSize = 0;
        for (const auto& stmt : blockStmt.body_) {
            if (stmt->kind_ == AST::NodeKind::ASSIGNMENT) {
                const auto& assignment = static_cast<AST::Assignment&>(*stmt);
                if (assignment.isDeclaration_) {
                    frameSize += 8;
                }
            }
        }
        return frameSize;
    }

    void stack_allocate_scope_variables(const AST::BlockStatement& blockStmt) {
        const int frameSize = get_current_scope_frame_size(blockStmt);
        if (frameSize != 0) {
            output_ << "    sub rsp, " << frameSize << "\n";
        }
    }

    void stack_deallocate_scope_variables(const AST::BlockStatement& blockStmt) {
        const int frameSize = get_current_scope_frame_size(blockStmt);
        if (frameSize != 0) {
            output_ << "    add rsp, " << frameSize << "\n";
        }
    }

    int get_variable_stack_offset(const std::string& name) const {
        return symbolTable_.at(name).stackOffset_;
    }

    void write_to_variable(const std::string& name, const std::string& source) {
        output_ << "    mov qword [rbp - " << get_variable_stack_offset(name) << "], " << source
                << "\n";
    }

    void move_variable_to_rax(const std::string& name) {
        output_ << "    mov rax, [rbp - " << get_variable_stack_offset(name) << "]\n";
    }

    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
        output_ << "    mov rax, " << numberLit.value_ << "\n";
    }

    void move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit) {
        output_ << "    mov rax, " << (booleanLit.value_ ? "1" : "0") << "\n";
    }

    void evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
        evaluate_expression_to_rax(*unaryExpr.operand_);
        if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
            output_ << "    neg rax\n";
        } else if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
            output_ << "    xor rax, 1\n";
        }
    }

    void evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr) {
        // First evaluate right side to prevent an additional move in case of division, because the
        // numerator has to be in rax
        evaluate_expression_to_rax(*binaryExpr.right_);
        output_ << "    push rax\n";

        evaluate_expression_to_rax(*binaryExpr.left_);

        output_ << "    pop rbx\n";

        const AST::Operator op = binaryExpr.operator_;

        if (AST::is_arithmetic_operator(op)) {
            if (op == AST::Operator::DIVIDE) {
                output_ << "    cqo\n";
                output_ << "    idiv rbx\n";
            } else {
                const std::unordered_map<AST::Operator, std::string> arithmeticOperatorPrefixes = {
                    {AST::Operator::ADD, "add"},
                    {AST::Operator::SUBTRACT, "sub"},
                    {AST::Operator::MULTIPLY, "imul"},
                };
                output_ << "    " << arithmeticOperatorPrefixes.at(op) << " rax, rbx\n";
            }

        } else if (AST::is_comparison_operator(op)) {
            const std::unordered_map<AST::Operator, std::string> comparisonSuffixes = {
                {AST::Operator::EQUALS, "e"},       {AST::Operator::NOT_EQUALS, "ne"},
                {AST::Operator::LESS_THAN, "l"},    {AST::Operator::LESS_THAN_OR_EQUAL, "le"},
                {AST::Operator::GREATER_THAN, "g"}, {AST::Operator::GREATER_THAN_OR_EQUAL, "ge"},
            };
            output_ << "    cmp rax, rbx\n";
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
            case AST::NodeKind::BOOLEAN_LITERAL: {
                const auto& booleanLit = static_cast<const AST::BooleanLiteral&>(expr);
                move_boolean_lit_to_rax(booleanLit);
                break;
            }
            case AST::NodeKind::IDENTIFIER: {
                const auto& identifier = static_cast<const AST::Identifier&>(expr);
                move_variable_to_rax(identifier.name_);
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

    void generate_assignment(const AST::Assignment& assignment) {
        const std::string& varName = assignment.identifier_->name_;
        evaluate_expression_to_rax(*assignment.value_);
        write_to_variable(varName, "rax");
    }

    void generate_if_stmt(const AST::IfStatement& ifStmt) {  // NOLINT(*-no-recursion)
        evaluate_expression_to_rax(*ifStmt.condition_);
        const std::string endifLabel = std::format(".endif{}", ifStmtsCount_++);
        output_ << "    cmp rax, 0\n";
        output_ << "    je " << endifLabel << "\n";
        generate_stmt(*ifStmt.body_);
        output_ << endifLabel << ":\n";
    }

    void generate_exit(const AST::Exit& exitStmt) {
        evaluate_expression_to_rax(*exitStmt.exitCode_);
        output_ << "    mov rdi, rax\n";  // exit code
        output_ << "    mov rax, 60\n";   // syscall: exit
        output_ << "    syscall\n";
    }

    void generate_stmt(const AST::Statement& stmt) {
        switch (stmt.kind_) {
            case AST::NodeKind::ASSIGNMENT: {
                const auto& assignmentStmt = static_cast<const AST::Assignment&>(stmt);
                generate_assignment(assignmentStmt);
                break;
            }
            case AST::NodeKind::IF_STATEMENT: {
                const auto& ifStmt = static_cast<const AST::IfStatement&>(stmt);
                generate_if_stmt(ifStmt);
                break;
            }
            case AST::NodeKind::EXIT: {
                const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
                generate_exit(exitStmt);
                break;
            }
            case AST::NodeKind::BLOCK_STATEMENT: {
                const auto& blockStmt = static_cast<const AST::BlockStatement&>(stmt);
                stack_allocate_scope_variables(blockStmt);
                for (const auto& innerStmt : blockStmt.body_) {
                    generate_stmt(*innerStmt);
                }
                stack_deallocate_scope_variables(blockStmt);
                break;
            }
            default:
                throw std::invalid_argument("Invalid statement kind");
        }
    }
};