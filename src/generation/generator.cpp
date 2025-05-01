#include "generation/generator.hpp"

#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>

#include "parsing/AST.hpp"

std::stringstream Generator::generate() {
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

int Generator::get_current_scope_frame_size(const AST::BlockStatement& blockStmt) const {
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

void Generator::stack_allocate_scope_variables(const AST::BlockStatement& blockStmt) {
    const int frameSize = get_current_scope_frame_size(blockStmt);
    if (frameSize != 0) {
        output_ << "    sub rsp, " << frameSize << "\n";
    }
}

void Generator::stack_deallocate_scope_variables(const AST::BlockStatement& blockStmt) {
    const int frameSize = get_current_scope_frame_size(blockStmt);
    if (frameSize != 0) {
        output_ << "    add rsp, " << frameSize << "\n";
    }
}

int Generator::get_variable_stack_offset(const std::string& name) const {
    return symbolTable_.at(name).stackOffset_.value();
}

void Generator::write_to_variable(const std::string& name, const std::string& source) {
    output_ << "    mov qword [rbp - " << get_variable_stack_offset(name) << "], " << source
            << "\n";
}

void Generator::move_variable_to_rax(const std::string& name) {
    output_ << "    mov rax, [rbp - " << get_variable_stack_offset(name) << "]\n";
}

void Generator::move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
    output_ << "    mov rax, " << numberLit.value_ << "\n";
}

void Generator::move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit) {
    output_ << "    mov rax, " << (booleanLit.value_ ? "1" : "0") << "\n";
}

void Generator::evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
    evaluate_expression_to_rax(*unaryExpr.operand_);
    if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
        output_ << "    neg rax\n";
    } else if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
        output_ << "    xor rax, 1\n";
    }
}

void Generator::evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr) {
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

void Generator::evaluate_expression_to_rax(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
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

int Generator::generate_condition(const AST::Expression& condition) {
    evaluate_expression_to_rax(condition);
    output_ << "    cmp rax, 0\n";
    output_ << "    je ." << labelsCount_++ << "\n";
    return labelsCount_ - 1;
}

void Generator::generate_assignment(const AST::Assignment& assignment) {
    const std::string& varName = assignment.identifier_->name_;
    evaluate_expression_to_rax(*assignment.value_);
    write_to_variable(varName, "rax");
}

void Generator::generate_if_stmt(const AST::IfStatement& ifStmt) {  // NOLINT(*-no-recursion)
    const int elseLabel = generate_condition(*ifStmt.condition_);
    generate_stmt(*ifStmt.body_);
    output_ << "." << elseLabel << ":\n";
}

void Generator::generate_while_stmt(const AST::WhileStatement& whileStmt) {
    const int whileLabel = labelsCount_++;
    output_ << "." << whileLabel << ":\n";
    const int endwhileLabel = generate_condition(*whileStmt.condition_);
    generate_stmt(*whileStmt.body_);
    output_ << "    jmp ." << whileLabel << "\n";
    output_ << "." << endwhileLabel << ":\n";
}

void Generator::generate_exit(const AST::Exit& exitStmt) {
    evaluate_expression_to_rax(*exitStmt.exitCode_);
    output_ << "    mov rdi, rax\n";  // exit code
    output_ << "    mov rax, 60\n";   // syscall: exit
    output_ << "    syscall\n";
}

void Generator::generate_stmt(const AST::Statement& stmt) {
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
        case AST::NodeKind::WHILE_STATEMENT: {
            const auto& whileStmt = static_cast<const AST::WhileStatement&>(stmt);
            generate_while_stmt(whileStmt);
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
            throw std::invalid_argument("Invalid statement kind at generation");
    }
}