#include "generation/generator.hpp"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>

#include "parsing/ast.hpp"

std::stringstream Generator::generate() {
    // Declare external functions
    for (const auto& externalFuncDecl : program_.externalFunctions_) {
        output_ << "extern " << function_name_with_prefix(externalFuncDecl->identifier_->name_)
                << "\n";
    }

    output_ << "\n";

    // Write the header
    output_ << "section .text\n";

    if (targetType_ == TargetType::EXECUTABLE) {
        output_ << "global _start\n";
        output_ << "_start:\n";
        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";
        output_ << "    call " << function_name_with_prefix("main") << "\n";
        generate_exit("0");
    }

    for (const auto& funcDef : program_.functions_) {
        generate_function_definition(*funcDef);
    }

    return std::move(output_);
}

int Generator::get_current_scope_frame_size(const AST::BlockStatement& blockStmt) {
    int frameSize = 0;
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            frameSize += 8;
        }
    }
    return frameSize;
}

void Generator::stack_allocate_scope_variables(const AST::BlockStatement& blockStmt) {
    const int frameSize = get_current_scope_frame_size(blockStmt);
    if (frameSize > 0) {
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
    assert(variablesStackOffset_.contains(name) && "Variable not found in stack offset map");

    return variablesStackOffset_.at(name);
}

void Generator::insert_variable_stack_offset(const std::string& name) {
    variablesStackOffset_.erase(name);
    variablesStackOffset_.emplace(name, currentStackOffset_);
    currentStackOffset_ += 8;
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

void Generator::write_array_to_heap(const AST::ArrayLiteral& arrayLit) {
    // Get the current break address
    output_ << "    mov rax, 12\n";
    output_ << "    xor rdi, rdi\n";
    output_ << "    syscall\n";

    // Store the address of the array base
    output_ << "    push rax\n";

    // Set the new break address
    output_ << "    add rax, " << (arrayLit.elements_.size() * 8) << "\n";  // 8 bytes per element
    output_ << "    mov rdi, rax\n";
    output_ << "    mov rax, 12\n";
    output_ << "    syscall\n";

    for (std::size_t i = 0; i < arrayLit.elements_.size(); ++i) {
        evaluate_expression_to_rax(*arrayLit.elements_[i]);
        output_ << "    mov rbx, [rsp]\n";  // Get the address of the allocated memory
        output_ << "    mov [rbx + " << (i * 8) << "], rax\n";  // Write each element to the array
    }

    output_ << "    pop rax\n";
}

void Generator::evaluate_array_access_address_to_rax(
    const AST::ArrayAccess& arrayAccess) {
    evaluate_expression_to_rax(*arrayAccess.base_);  // Pointer to the array
    output_ << "    push rax\n";
    evaluate_expression_to_rax(*arrayAccess.index_);
    output_ << "    pop rbx\n";
    output_ << "    shl rax, 3\n";    // Multiply index by 8 (size of qword)
    output_ << "    add rax, rbx\n";  // Add the base
}

void Generator::evaluate_array_access_to_rax(
    const AST::ArrayAccess& arrayAccess) {
    evaluate_array_access_address_to_rax(arrayAccess);
    output_ << "    mov rax, [rax]\n";  // Dereference
}

std::string Generator::function_name_with_prefix(const std::string& name) {
    return "__" + name;  // Prefix with "__" to avoid conflicts with NASM keywords
}

void Generator::generate_function_call(
    const AST::FunctionCall& funcCall) {
    for (const auto& argument : funcCall.arguments_) {
        evaluate_expression_to_rax(*argument);
        output_ << "    push rax\n";
    }

    output_ << "    call " << function_name_with_prefix(funcCall.callee_->name_) << "\n";

    // Clean up the stack after the function call
    for (int i = 0; i < funcCall.arguments_.size(); ++i) {
        output_ << "    pop rcx\n";
    }
}

void Generator::evaluate_unary_expression_to_rax(
    const AST::UnaryExpression& unaryExpr) {
    evaluate_expression_to_rax(*unaryExpr.operand_);
    if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
        output_ << "    neg rax\n";
    } else if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
        output_ << "    xor rax, 1\n";
    }
}

void Generator::evaluate_binary_expression_to_rax(
    const AST::BinaryExpression& binaryExpr) {
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

void Generator::evaluate_expression_to_rax(const AST::Expression& expr) {
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
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLit = static_cast<const AST::ArrayLiteral&>(expr);
            write_array_to_heap(arrayLit);
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            move_variable_to_rax(identifier.name_);
            break;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(expr);
            evaluate_array_access_to_rax(arrayAccess);
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = static_cast<const AST::FunctionCall&>(expr);
            generate_function_call(funcCall);
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
            std::unreachable();
    }
}

[[nodiscard]] int Generator::generate_condition(const AST::Expression& condition) {
    evaluate_expression_to_rax(condition);
    output_ << "    cmp rax, 0\n";
    output_ << "    je ." << labelsCount_++ << "\n";
    return labelsCount_ - 1;
}

void Generator::generate_variable_definition(const AST::VariableDefinition& varDecl) {
    const std::string& varName = varDecl.identifier_->name_;
    insert_variable_stack_offset(varName);

    evaluate_expression_to_rax(*varDecl.value_);
    write_to_variable(varName, "rax");
}

void Generator::generate_variable_assignment(const AST::Assignment& assignment) {
    const auto& place = assignment.place_;
    const auto& value = assignment.value_;
    evaluate_expression_to_rax(*value);

    if (place->kind_ == AST::NodeKind::IDENTIFIER) {
        const auto& identifier = static_cast<const AST::Identifier&>(*place);
        write_to_variable(identifier.name_, "rax");
    } else if (place->kind_ == AST::NodeKind::ARRAY_ACCESS) {
        const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(*place);
        output_ << "    push rax\n";
        evaluate_array_access_address_to_rax(arrayAccess);
        output_ << "    pop rbx\n";
        output_ << "    mov [rax], rbx\n";
    } else {
        std::unreachable();
    }
}

void Generator::generate_expression_stmt(const AST::ExpressionStatement& exprStmt) {
    evaluate_expression_to_rax(*exprStmt.expression_);
}

void Generator::generate_if_stmt(const AST::IfStatement& ifStmt) {
    const int elseLabel = generate_condition(*ifStmt.condition_);
    const int endifLabel = ifStmt.elseClause_ ? labelsCount_++ : elseLabel;

    generate_stmt(*ifStmt.body_);

    if (ifStmt.elseClause_) {
        output_ << "    jmp ." << endifLabel << "\n";

        output_ << "." << elseLabel << ":\t; else\n";
        generate_stmt(*ifStmt.elseClause_);
    }

    output_ << "." << endifLabel << ":\t; endif\n";
}

void Generator::generate_while_stmt(
    const AST::WhileStatement& whileStmt) {
    const int whileLabel = labelsCount_++;
    innerLoopStartLabel_ = whileLabel;
    output_ << "." << whileLabel << ":\n";

    const int endwhileLabel = generate_condition(*whileStmt.condition_);
    innerLoopEndLabel_ = endwhileLabel;

    generate_stmt(*whileStmt.body_);
    output_ << "    jmp ." << whileLabel << "\n";
    output_ << "." << endwhileLabel << ":\n";
}

void Generator::generate_break_statement() { output_ << "    jmp ." << innerLoopEndLabel_ << "\n"; }

void Generator::generate_continue_statement() {
    output_ << "    jmp ." << innerLoopStartLabel_ << "\n";
}

void Generator::generate_return_statement(const AST::ReturnStatement& returnStmt) {
    if (returnStmt.returnValue_) {
        evaluate_expression_to_rax(*returnStmt.returnValue_);
    }
    output_ << "    leave\n";
    output_ << "    ret\n";
}

void Generator::generate_exit(const std::string& source) {
    output_ << "    mov rdi, " << source << "\n";  // exit code
    output_ << "    mov rax, 60\n";                // syscall: exit
    output_ << "    syscall\n";
}
void Generator::generate_exit(const AST::ExitStatement& exitStmt) {
    evaluate_expression_to_rax(*exitStmt.exitCode_);
    generate_exit("rax");
}

void Generator::generate_stmt(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = static_cast<const AST::VariableDefinition&>(stmt);
            generate_variable_definition(varDecl);
            break;
        }
        case AST::NodeKind::ASSIGNMENT: {
            const auto& assignment = static_cast<const AST::Assignment&>(stmt);
            generate_variable_assignment(assignment);
            break;
        }
        case AST::NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = static_cast<const AST::ExpressionStatement&>(stmt);
            generate_expression_stmt(exprStmt);
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
        case AST::NodeKind::BREAK_STATEMENT:
            generate_break_statement();
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            generate_continue_statement();
            break;
        case AST::NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = static_cast<const AST::ReturnStatement&>(stmt);
            generate_return_statement(returnStmt);
            break;
        }
        case AST::NodeKind::EXIT_STATEMENT: {
            const auto& exitStmt = static_cast<const AST::ExitStatement&>(stmt);
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
            std::unreachable();
    }
}

void Generator::generate_function_definition(const AST::FunctionDefinition& funcDef) {
    output_ << "\n";

    if (funcDef.isExported_) {
        output_ << "global " << function_name_with_prefix(funcDef.identifier_->name_) << "\n";
    }

    output_ << function_name_with_prefix(funcDef.identifier_->name_)
            << ":\n";  // Prefix with "__" to avoid conflicts with NASM keywords

    currentStackOffset_ = INITIAL_STACK_OFFSET;

    output_ << "    push rbp\n";        // Save the old base pointer
    output_ << "    mov rbp, rsp\n\n";  // Set the new base pointer

    const std::size_t parametersFrameSize = funcDef.parameters_.size() * 8;
    if (parametersFrameSize > 0) {
        output_ << "    sub rsp, " << parametersFrameSize << "\n";

        for (std::size_t i = 0; i < funcDef.parameters_.size(); ++i) {
            const auto& param = funcDef.parameters_[i];
            const std::size_t paramOffset = parametersFrameSize + 8 - i * 8;
            output_ << "    mov rax, [rbp + " << paramOffset << "]\n";

            insert_variable_stack_offset(param->identifier_->name_);
            write_to_variable(param->identifier_->name_, "rax");
        }

        output_ << "\n";
    }

    generate_stmt(*funcDef.body_);

    if (funcDef.returnType_.matches(PrimitiveType::VOID)) {
        output_ << "\n";
        output_ << "    xor rax, rax\n";  // Return 0
        output_ << "    leave\n";
        output_ << "    ret\n";
    } else {
        output_ << "    ud2\n";  // Prevent fallthrough
    }
}
