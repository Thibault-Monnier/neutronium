#include "generation/generator.hpp"

#include <cassert>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>

#include "parsing/ast.hpp"

using namespace CodeGen;

std::stringstream Generator::generate() {
    // Declare external functions
    for (const auto& externalFuncDecl : program_.externalFunctions_) {
        output_ << "extern " << function_name_with_prefix(externalFuncDecl->identifier_->name_)
                << "\n";
    }

    if (!program_.externalFunctions_.empty()) output_ << "\n";

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

int Generator::exprRealSizeBits(const AST::Expression& expr) const {
    const Type& type = typeManager_.getType(expr.typeID_);
    return type.sizeBits(typeManager_);
}

int Generator::typeStackSizeBits(const Type& type) const {
    // Arrays are stored as 8-byte pointers to the heap
    return type.isArray() ? 64 : type.sizeBits(typeManager_);
}

int Generator::exprStackSizeBits(const AST::Expression& expr) const {
    const Type& type = typeManager_.getType(expr.typeID_);
    return typeStackSizeBits(type);
}

void Generator::insert_symbol(const std::string& name, const TypeID typeID) {
    symbolTable_.erase(name);

    const Type& type = typeManager_.getType(typeID);
    assert(!type.isVoid() && "Void type cannot be stored in a variable");

    const int stackSizeBits = typeStackSizeBits(type);

    currentStackOffset_ += stackSizeBits / 8;

    const SymbolInfo info = {
        .name_ = name, .stackOffset_ = currentStackOffset_, .stackSizeBits_ = stackSizeBits};
    symbolTable_.emplace(name, info);
}

int Generator::get_scope_frame_size(const AST::BlockStatement& blockStmt) const {
    int frameSize = 0;
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            const auto& varDef = static_cast<const AST::VariableDefinition&>(*stmt);
            frameSize += get_variable_size_bits(varDef.identifier_->name_) / 8;
        }
    }
    return frameSize;
}

void Generator::enter_scope(const AST::BlockStatement& blockStmt) {
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            const auto& varDef = static_cast<const AST::VariableDefinition&>(*stmt);
            insert_symbol(varDef.identifier_->name_, varDef.typeID_);
        }
    }

    const int frameSize = get_scope_frame_size(blockStmt);
    if (frameSize > 0) {
        output_ << "    sub rsp, " << frameSize << "\n";
    }
}

void Generator::exit_scope(const AST::BlockStatement& blockStmt) {
    const int frameSize = get_scope_frame_size(blockStmt);
    if (frameSize > 0) {
        output_ << "    add rsp, " << frameSize << "\n";
    }
}

int Generator::get_variable_size_bits(const std::string& name) const {
    return symbolTable_.at(name).stackSizeBits_;
}

int Generator::get_variable_stack_offset(const std::string& name) const {
    assert(symbolTable_.contains(name) && "Variable not found in stack offset map");

    return symbolTable_.at(name).stackOffset_;
}

std::string_view Generator::size_directive(const int bitSize) {
    switch (bitSize) {
        case 8:
            return "byte";
        case 16:
            return "word";
        case 32:
            return "dword";
        case 64:
            return "qword";
        default:
            std::unreachable();
    }
}

std::string_view Generator::register_a_for_size(const int bitSize) {
    switch (bitSize) {
        case 8:
            return "al";
        case 16:
            return "ax";
        case 32:
            return "eax";
        case 64:
            return "rax";
        default:
            std::unreachable();
    }
}

void Generator::clean_rax(const int raxValueSizeBits) {
    switch (raxValueSizeBits) {
        case 8:
        case 16:
            output_ << "    movzx rax, " << register_a_for_size(raxValueSizeBits) << "\n";
            break;
        case 32:
        case 64:
            break;
        default:
            std::unreachable();
    }
}

void Generator::write_to_variable(const std::string& name, const std::string_view source) {
    const int size = get_variable_size_bits(name);
    const int stackOffset = get_variable_stack_offset(name);
    output_ << "    mov " << size_directive(size) << " [rbp - " << stackOffset << "], " << source
            << "\n";
}

void Generator::move_variable_to_rax(const std::string& name) {
    const int sizeBits = get_variable_size_bits(name);
    const int stackOffset = get_variable_stack_offset(name);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", [rbp - " << stackOffset << "]\n";
    clean_rax(sizeBits);
}

void Generator::move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
    const int sizeBits = exprStackSizeBits(numberLit);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", " << numberLit.value_ << "\n";
    clean_rax(sizeBits);
}

void Generator::move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit) {
    const int sizeBits = exprStackSizeBits(booleanLit);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", "
            << (booleanLit.value_ ? "1" : "0") << "\n";
    clean_rax(sizeBits);
}

void Generator::evaluate_array_access_address_to_rax(const AST::ArrayAccess& arrayAccess) {
    evaluate_expression_to_rax(*arrayAccess.base_);  // Pointer to the array
    output_ << "    push rax\n";
    evaluate_expression_to_rax(*arrayAccess.index_);
    output_ << "    pop rbx\n";

    const int elementSizeBits = exprStackSizeBits(arrayAccess);
    output_ << "    imul rax, " << (elementSizeBits / 8) << "\n";  // Multiply index by element size
    output_ << "    add rax, rbx\n";                               // Add the base
}

void Generator::evaluate_place_expression_address_to_rax(const AST::Expression& place) {
    if (place.kind_ == AST::NodeKind::IDENTIFIER) {
        const auto& identifier = static_cast<const AST::Identifier&>(place);
        output_ << "    mov rax, rbp\n";
        output_ << "    sub rax, " << get_variable_stack_offset(identifier.name_) << "\n";
    } else if (place.kind_ == AST::NodeKind::ARRAY_ACCESS) {
        const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(place);
        evaluate_array_access_address_to_rax(arrayAccess);
    } else {
        std::unreachable();
    }
}

void Generator::write_array_to_heap(const AST::ArrayLiteral& arrayLit) {
    // Get the current break address
    output_ << "    mov rax, 12\n";
    output_ << "    xor rdi, rdi\n";
    output_ << "    syscall\n";

    // Store the address of the array base (1)
    output_ << "    push rax\n";

    // Set the new break address
    const int arraySizeBits = exprRealSizeBits(arrayLit);
    output_ << "    add rax, " << arraySizeBits / 8 << "\n";
    output_ << "    mov rdi, rax\n";
    output_ << "    mov rax, 12\n";
    output_ << "    syscall\n";

    const int elementSizeBits = exprStackSizeBits(arrayLit);
    for (int i = 0; i < static_cast<int>(arrayLit.elements_.size()); ++i) {
        const auto& element = arrayLit.elements_[i];

        evaluate_expression_to_rax(*element);
        // Get the address of the allocated memory (1)
        output_ << "    mov rbx, [rsp]\n";

        // Write the element to the heap
        output_ << "    mov [rbx + " << elementSizeBits / 8 * i << "], "
                << register_a_for_size(elementSizeBits) << "\n";
    }

    // Retrieve the address of the array base (1)
    output_ << "    pop rax\n";
}

void Generator::evaluate_array_access_to_rax(const AST::ArrayAccess& arrayAccess) {
    evaluate_array_access_address_to_rax(arrayAccess);
    const int elementSizeBits = exprStackSizeBits(arrayAccess);
    output_ << "    mov " << register_a_for_size(elementSizeBits) << ", [rax]\n";  // Dereference
}

std::string Generator::function_name_with_prefix(const std::string& name) {
    return "__" + name;  // Prefix with "__" to avoid conflicts with NASM keywords
}

void Generator::generate_function_call(const AST::FunctionCall& funcCall) {
    for (const auto& argument : funcCall.arguments_) {
        evaluate_expression_to_rax(*argument);

        static_assert(FUNCTION_ARGUMENT_SIZE_BITS == 64,
                      "Function argument size must be 64 bits to match the calling convention");
        output_ << "    push rax\n";
    }

    output_ << "    call " << function_name_with_prefix(funcCall.callee_->name_) << "\n";

    // Clean up the stack
    if (funcCall.arguments_.size() > 0) {
        output_ << "    add rsp, " << funcCall.arguments_.size() * (FUNCTION_ARGUMENT_SIZE_BITS / 8)
                << "\n";
    }
}

void Generator::evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
    evaluate_expression_to_rax(*unaryExpr.operand_);

    const int operandSizeBits = exprStackSizeBits(*unaryExpr.operand_);
    const std::string_view reg = register_a_for_size(operandSizeBits);
    if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
        output_ << "    neg " << reg << "\n";
    } else if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
        output_ << "    xor " << reg << ", 1\n";
    }
}

void Generator::apply_arithmetic_operator_to_rax(const AST::Operator op, const std::string& other) {
    if (op == AST::Operator::DIVIDE) {
        output_ << "    cqo\n";
        output_ << "    idiv " << other << "\n";
    } else {
        const std::unordered_map<AST::Operator, std::string> arithmeticOperatorPrefixes = {
            {AST::Operator::ADD, "add"},
            {AST::Operator::SUBTRACT, "sub"},
            {AST::Operator::MULTIPLY, "imul"},
        };
        assert(arithmeticOperatorPrefixes.contains(op) && "Unsupported arithmetic operator");
        output_ << "    " << arithmeticOperatorPrefixes.at(op) << " rax, " << other << "\n";
    }
}

void Generator::evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr) {
    // First evaluate right side to prevent an additional move in case of division, because the
    // numerator has to be in rax
    evaluate_expression_to_rax(*binaryExpr.right_);
    const int rightSizeBits = exprStackSizeBits(*binaryExpr.right_);
    clean_rax(rightSizeBits);
    output_ << "    push rax\n";

    evaluate_expression_to_rax(*binaryExpr.left_);
    const int leftSizeBits = exprStackSizeBits(*binaryExpr.left_);
    clean_rax(leftSizeBits);

    output_ << "    pop rbx\n";

    const AST::Operator op = binaryExpr.operator_;

    if (AST::is_arithmetic_operator(op)) {
        apply_arithmetic_operator_to_rax(op, "rbx");
    } else if (AST::is_comparison_operator(op)) {
        const std::unordered_map<AST::Operator, std::string> comparisonSuffixes = {
            {AST::Operator::EQUALS, "e"},       {AST::Operator::NOT_EQUALS, "ne"},
            {AST::Operator::LESS_THAN, "l"},    {AST::Operator::LESS_THAN_OR_EQUAL, "le"},
            {AST::Operator::GREATER_THAN, "g"}, {AST::Operator::GREATER_THAN_OR_EQUAL, "ge"},
        };
        output_ << "    cmp rax, rbx\n";
        output_ << "    set" << comparisonSuffixes.at(op) << " al\n";
        output_ << "    movzx rax, al\n";
    } else {
        std::unreachable();
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
    const int sizeBits = get_variable_size_bits(varName);
    evaluate_expression_to_rax(*varDecl.value_);
    write_to_variable(varName, register_a_for_size(sizeBits));
}

void Generator::generate_variable_assignment(const AST::Assignment& assignment) {
    const auto& place = assignment.place_;
    const auto& value = assignment.value_;

    AST::Operator op = {};
    if (assignment.operator_ == AST::Operator::ASSIGN) {
    } else if (assignment.operator_ == AST::Operator::ADD_ASSIGN)
        op = AST::Operator::ADD;
    else if (assignment.operator_ == AST::Operator::SUBTRACT_ASSIGN)
        op = AST::Operator::SUBTRACT;
    else if (assignment.operator_ == AST::Operator::MULTIPLY_ASSIGN)
        op = AST::Operator::MULTIPLY;
    else if (assignment.operator_ == AST::Operator::DIVIDE_ASSIGN)
        op = AST::Operator::DIVIDE;
    else
        std::unreachable();

    evaluate_place_expression_address_to_rax(*place);
    output_ << "    push rax\n";

    evaluate_expression_to_rax(*value);
    const int valueSizeBits = exprStackSizeBits(*value);
    clean_rax(valueSizeBits);

    if (assignment.operator_ == AST::Operator::ASSIGN) {
        output_ << "    pop rbx\n";
        output_ << "    mov [rbx], rax\n";  // Write the result back to the place
    } else {
        output_ << "    mov rbx, rax\n";
        output_ << "    pop rcx\n";
        output_ << "    mov rax, [rcx]\n";

        apply_arithmetic_operator_to_rax(op, "rbx");
        output_ << "    mov [rcx], " << register_a_for_size(valueSizeBits) << "\n";
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

void Generator::generate_while_stmt(const AST::WhileStatement& whileStmt) {
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
            enter_scope(blockStmt);
            for (const auto& innerStmt : blockStmt.body_) {
                generate_stmt(*innerStmt);
            }
            exit_scope(blockStmt);
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

    output_ << function_name_with_prefix(funcDef.identifier_->name_) << ":\n";

    currentStackOffset_ = INITIAL_STACK_OFFSET;

    output_ << "    push rbp\n";        // Save the old base pointer
    output_ << "    mov rbp, rsp\n\n";  // Set the new base pointer

    for (const auto& param : funcDef.parameters_) {
        insert_symbol(param->identifier_->name_, param->typeID_);
    }

    if (funcDef.parameters_.size() > 0) {
        static_assert(FUNCTION_ARGUMENT_SIZE_BITS == 64 &&
                      "Function argument size must be 64 bits to match the calling convention");
        output_ << "    sub rsp, " << funcDef.parameters_.size() * (FUNCTION_ARGUMENT_SIZE_BITS / 8)
                << "\n";  // Allocate space for parameters
    }

    int currentParamOffset = 16;  // [rbp] is the saved rbp, [rbp + 8] is the return address
    for (const auto& param : std::views::reverse(funcDef.parameters_)) {
        static_assert(FUNCTION_ARGUMENT_SIZE_BITS == 64,
                      "Function argument size must be 64 bits to match the calling convention");
        output_ << "    mov rax, [rbp + " << currentParamOffset << "]\n";

        const int sizeBits = get_variable_size_bits(param->identifier_->name_);
        write_to_variable(param->identifier_->name_, register_a_for_size(sizeBits));

        currentParamOffset += FUNCTION_ARGUMENT_SIZE_BITS / 8;
    }

    if (funcDef.parameters_.size() > 0) {
        output_ << "\n";
    }

    generate_stmt(*funcDef.body_);

    const Type& returnType = typeManager_.getType(funcDef.returnTypeID_);
    if (returnType.isVoid()) {
        output_ << "\n";
        output_ << "    xor rax, rax\n";  // Return 0
        output_ << "    leave\n";
        output_ << "    ret\n";
    } else {
        output_ << "    ud2\n";  // Prevent fallthrough
    }
}
