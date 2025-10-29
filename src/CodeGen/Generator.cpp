#include "CodeGen/Generator.hpp"

#include <cassert>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>

#include "Parser/AST.hpp"

using namespace CodeGen;

std::stringstream Generator::generate() {
    output_ << ".intel_syntax noprefix\n\n";

    // Declare external functions
    for (const auto& externalFuncDecl : program_.externalFunctions_) {
        output_ << ".extern " << function_name_with_prefix(externalFuncDecl->identifier_->name_)
                << "\n";
    }

    if (!program_.externalFunctions_.empty()) output_ << "\n";

    // Write the header
    output_ << ".text\n";

    if (targetType_ == TargetType::EXECUTABLE) {
        output_ << ".globl _start\n";
        output_ << ".type _start, @function\n";
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

void Generator::insert_symbol(const std::string& name, const TypeID typeID) {
    symbolTable_.erase(name);

    const Type& type = typeManager_.getType(typeID);
    assert(!type.isVoid() && "Void type cannot be stored in a variable");

    const int sizeBits = typeSizeBits(type);

    currentStackOffset_ += sizeBits / 8;

    const SymbolInfo info = {.name_ = name,
                             .stackOffset_ = currentStackOffset_,
                             .stackSizeBits_ = sizeBits,
                             .typeID_ = typeID};
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
        // No need to update currentStackOffset_ here, as it has already been updated in
        // insert_symbol
        output_ << "    sub rsp, " << frameSize << "\n";
    }
}

void Generator::exit_scope(const AST::BlockStatement& blockStmt) {
    const int frameSize = get_scope_frame_size(blockStmt);
    if (frameSize > 0) {
        free_stack_space(frameSize * 8);
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

std::string Generator::label(const int labelID) { return std::format(".L{}", labelID); }

void Generator::clean_rax(const int raxValueSizeBits) {
    switch (raxValueSizeBits) {
        case 8:
        case 16:
            output_ << "    movzx rax, " << register_a_for_size(raxValueSizeBits) << "\n";
            break;
        default:
            break;
    }
}

void Generator::load_value_from_rax(const int bitSize) {
    output_ << "    mov " << register_a_for_size(bitSize) << ", [rax]\n";
    clean_rax(bitSize);
}

void Generator::push(const std::string_view reg, const int sizeBits) {
    output_ << "    push " << reg << "\n";
    currentStackOffset_ += sizeBits / 8;
}

void Generator::pop(const std::string_view reg, const int sizeBits) {
    output_ << "    pop " << reg << "\n";
    currentStackOffset_ -= sizeBits / 8;
}

void Generator::allocate_stack_space(const int sizeBits) {
    output_ << "    sub rsp, " << (sizeBits / 8) << "\n";
    currentStackOffset_ += sizeBits / 8;
}

void Generator::free_stack_space(const int sizeBits) {
    output_ << "    add rsp, " << (sizeBits / 8) << "\n";
    currentStackOffset_ -= sizeBits / 8;
}

std::string Generator::stack_top_memory_operand() const {
    return "[rbp - " + std::to_string(currentStackOffset_) + "]";
}

void Generator::write_to_variable_from_rax(const std::string& name) {
    const Type& type = typeManager_.getType(symbolTable_.at(name).typeID_);
    const int size = get_variable_size_bits(name);
    const int stackOffset = get_variable_stack_offset(name);

    switch (type.kind()) {
        case TypeKind::PRIMITIVE:
            output_ << "    mov [rbp - " << stackOffset << "], " << register_a_for_size(size)
                    << "\n";
            break;
        case TypeKind::ARRAY:
            output_ << "    mov rbx, rbp\n";
            output_ << "    sub rbx, " << stackOffset << "\n";
            copy_array_contents("rax", "rbx", size);
            break;
        default:
            std::unreachable();
    }
}

void Generator::move_variable_to_rax(const std::string& name) {
    const int sizeBits = get_variable_size_bits(name);
    const int stackOffset = get_variable_stack_offset(name);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", [rbp - " << stackOffset << "]\n";
    clean_rax(sizeBits);
}

void Generator::move_number_lit_to_rax(const AST::NumberLiteral& numberLit) {
    const int sizeBits = exprSizeBits(numberLit);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", " << numberLit.value_ << "\n";
    clean_rax(sizeBits);
}

void Generator::move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit) {
    const int sizeBits = exprSizeBits(booleanLit);
    output_ << "    mov " << register_a_for_size(sizeBits) << ", "
            << (booleanLit.value_ ? "1" : "0") << "\n";
    clean_rax(sizeBits);
}

void Generator::evaluate_place_expression_address_to_rax(const AST::Expression& place) {
    if (place.kind_ == AST::NodeKind::IDENTIFIER) {
        const auto& identifier = static_cast<const AST::Identifier&>(place);
        output_ << "    mov rax, rbp\n";
        output_ << "    sub rax, " << get_variable_stack_offset(identifier.name_) << "\n";

    } else if (place.kind_ == AST::NodeKind::ARRAY_ACCESS) {
        const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(place);

        evaluate_expression_to_rax(*arrayAccess.base_);
        push("rax");
        evaluate_expression_to_rax(*arrayAccess.index_);
        pop("rbx");

        const int sizeBits = exprSizeBits(arrayAccess);
        output_ << "    imul rax, " << (sizeBits / 8) << "\n";  // Multiply index by element size
        output_ << "    add rax, rbx\n";                        // Add the base address

    } else {
        std::unreachable();
    }
}

void Generator::generate_array_lit(const AST::ArrayLiteral& arrayLit,
                                   const std::string_view destinationAddress) {
    output_ << "    mov rbx, " << destinationAddress << "\n";

    const size_t elementSizeBits = exprSizeBits(arrayLit) / arrayLit.elements_.size();
    for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
        if (i != 0) output_ << "    add rbx, " << elementSizeBits / 8 << "\n";
        push("rbx");
        generate_expression(*arrayLit.elements_[i], stack_top_memory_operand());
        pop("rbx");
    }

    output_ << "    mov rax, " << destinationAddress << "\n";
}

void Generator::allocate_and_generate_array_literal(const AST::ArrayLiteral& arrayLit) {
    allocate_stack_space(exprSizeBits(arrayLit));
    push("rsp");
    generate_array_lit(arrayLit, stack_top_memory_operand());
    pop("rbx");
}

std::string Generator::function_name_with_prefix(const std::string& name) {
    return "__" + name;  // Prefix with "__" to avoid conflicts with NASM keywords
}

void Generator::generate_function_call(const AST::FunctionCall& funcCall,
                                       const std::optional<std::string_view>& destinationAddress) {
    for (const auto& argument : funcCall.arguments_) {
        evaluate_expression_to_rax(*argument);
        push("rax", FUNCTION_ARGUMENT_SIZE_BITS);
    }

    output_ << "    call " << function_name_with_prefix(funcCall.callee_->name_) << "\n";

    // Clean up the stack
    if (funcCall.arguments_.size() > 0) {
        free_stack_space(funcCall.arguments_.size() * FUNCTION_ARGUMENT_SIZE_BITS);
    }

    if (!destinationAddress.has_value()) {
        return;
    }

    const Type& returnType = typeManager_.getType(funcCall.typeID_);
    const int sizeBits = typeSizeBits(returnType);
    switch (returnType.kind()) {
        case TypeKind::PRIMITIVE:
            clean_rax(sizeBits);
            output_ << "    mov rbx, " << destinationAddress.value() << "\n";
            output_ << "    mov [rbx], " << register_a_for_size(sizeBits) << "\n";
            break;
        case TypeKind::ARRAY:
            copy_array_contents("rax", destinationAddress.value(), sizeBits);
            break;
        default:
            std::unreachable();
    }
}

void Generator::allocate_and_generate_function_call(const AST::FunctionCall& funcCall) {
    assert(typeManager_.getType(funcCall.typeID_).kind() == TypeKind::ARRAY &&
           "Only function calls returning arrays require allocation");
    allocate_stack_space(exprSizeBits(funcCall));
    push("rsp");
    generate_function_call(funcCall, stack_top_memory_operand());
    pop("rbx");
}

void Generator::evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr) {
    evaluate_expression_to_rax(*unaryExpr.operand_);

    const int operandSizeBits = exprSizeBits(*unaryExpr.operand_);
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
    const int rightSizeBits = exprSizeBits(*binaryExpr.right_);
    clean_rax(rightSizeBits);
    push("rax");

    evaluate_expression_to_rax(*binaryExpr.left_);
    const int leftSizeBits = exprSizeBits(*binaryExpr.left_);
    clean_rax(leftSizeBits);

    pop("rbx");

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

void Generator::generate_primitive_expression(
    const AST::Expression& expr, const std::optional<std::string_view>& destinationAddress) {
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
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(expr);
            evaluate_place_expression_address_to_rax(arrayAccess);
            load_value_from_rax(exprSizeBits(expr));
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = static_cast<const AST::FunctionCall&>(expr);
            generate_function_call(funcCall, destinationAddress);
            return;
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

    if (destinationAddress.has_value()) {
        const int sizeBits = exprSizeBits(expr);
        output_ << "    mov rbx, " << destinationAddress.value() << "\n";
        output_ << "    mov [rbx], " << register_a_for_size(sizeBits) << "\n";
    }
}

void Generator::generate_array_expression(
    const AST::Expression& expr, const std::optional<std::string_view>& destinationAddress) {
    if (expr.kind_ == AST::NodeKind::ARRAY_LITERAL) {
        const auto& arrayLit = static_cast<const AST::ArrayLiteral&>(expr);
        if (destinationAddress.has_value()) {
            generate_array_lit(arrayLit, destinationAddress.value());
        } else {
            allocate_and_generate_array_literal(arrayLit);
        }

    } else if (expr.kind_ == AST::NodeKind::FUNCTION_CALL) {
        const auto& funcCall = static_cast<const AST::FunctionCall&>(expr);
        if (destinationAddress.has_value()) {
            generate_function_call(funcCall, destinationAddress);
        } else {
            allocate_and_generate_function_call(funcCall);
        }

    } else {
        evaluate_place_expression_address_to_rax(expr);
        if (destinationAddress.has_value()) {
            copy_array_contents("rax", destinationAddress.value(), exprSizeBits(expr));
        } else {
            // Do nothing: the result is already in rax
        }
    }
}

void Generator::generate_expression(const AST::Expression& expr,
                                    const std::optional<std::string_view>& destinationAddress) {
    const Type& exprType = typeManager_.getType(expr.typeID_);
    switch (exprType.kind()) {
        case TypeKind::PRIMITIVE:
            generate_primitive_expression(expr, destinationAddress);
            break;

        case TypeKind::ARRAY:
            generate_array_expression(expr, destinationAddress);
            break;

        default:
            std::unreachable();
    }
}

void Generator::copy_array_contents(const std::string_view sourceAddress,
                                    const std::string_view destinationAddress,
                                    const int arraySizeBits) {
    output_ << "    mov rsi, " << sourceAddress << "\n";
    output_ << "    mov rdi, " << destinationAddress << "\n";
    output_ << "    mov rcx, " << (arraySizeBits / 8) << "\n";
    output_ << "    rep movsb\n";
}

[[nodiscard]] int Generator::generate_condition(const AST::Expression& condition) {
    evaluate_expression_to_rax(condition);
    output_ << "    cmp rax, 0\n";
    output_ << "    je " << label(labelsCount_) << "\n";
    return labelsCount_++;
}

void Generator::generate_variable_definition(const AST::VariableDefinition& varDecl) {
    evaluate_place_expression_address_to_rax(*varDecl.identifier_);
    push("rax");
    generate_expression(*varDecl.value_, stack_top_memory_operand());
    pop("rbx");
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
    push("rax");

    if (assignment.operator_ == AST::Operator::ASSIGN) {
        generate_expression(*value, stack_top_memory_operand());
        pop("rcx");
    } else {
        const int sizeBits = exprSizeBits(*place);
        assert(sizeBits <= 64 && "Compound assignment must have <= 64-bit size");

        evaluate_expression_to_rax(*value);

        output_ << "    mov rbx, rax\n";
        pop("rcx");
        output_ << "    mov rax, [rcx]\n";

        apply_arithmetic_operator_to_rax(op, "rbx");
        output_ << "    mov [rcx], " << register_a_for_size(sizeBits) << "\n";
    }
}

void Generator::generate_expression_stmt(const AST::ExpressionStatement& exprStmt) {
    const auto& expr = *exprStmt.expression_;
    evaluate_expression_to_rax(expr);
}

void Generator::generate_if_stmt(const AST::IfStatement& ifStmt) {
    const std::string elseLabel = label(generate_condition(*ifStmt.condition_));
    const std::string endifLabel = ifStmt.elseClause_ ? label(labelsCount_++) : elseLabel;

    generate_stmt(*ifStmt.body_);

    if (ifStmt.elseClause_) {
        output_ << "    jmp " << endifLabel << "\n";

        output_ << elseLabel << ":\n";
        generate_stmt(*ifStmt.elseClause_);
    }

    output_ << endifLabel << ":\n";
}

void Generator::generate_while_stmt(const AST::WhileStatement& whileStmt) {
    const std::string whileLabel = label(labelsCount_++);
    innerLoopStartLabel_ = whileLabel;
    output_ << whileLabel << ":\n";

    const std::string endwhileLabel = label(generate_condition(*whileStmt.condition_));
    innerLoopEndLabel_ = endwhileLabel;

    generate_stmt(*whileStmt.body_);
    output_ << "    jmp " << whileLabel << "\n";
    output_ << endwhileLabel << ":\n";
}

void Generator::generate_break_statement() { output_ << "    jmp " << innerLoopEndLabel_ << "\n"; }

void Generator::generate_continue_statement() {
    output_ << "    jmp " << innerLoopStartLabel_ << "\n";
}

void Generator::generate_return_statement(const AST::ReturnStatement& returnStmt) {
    evaluate_expression_to_rax(*returnStmt.returnValue_);
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

    const std::string& funcName = function_name_with_prefix(funcDef.identifier_->name_);
    if (funcDef.isExported_) {
        output_ << ".globl " << funcName << "\n";
        output_ << ".type " << funcName << ", @function\n";
    }

    output_ << funcName << ":\n";

    currentStackOffset_ = INITIAL_STACK_OFFSET;

    output_ << "    push rbp\n";        // Save the old base pointer
    output_ << "    mov rbp, rsp\n\n";  // Set the new base pointer

    for (const auto& param : funcDef.parameters_) {
        insert_symbol(param->identifier_->name_, param->typeID_);
    }

    int paramsSizeBits = 0;
    for (const auto& param : funcDef.parameters_)
        paramsSizeBits += get_variable_size_bits(param->identifier_->name_);

    if (paramsSizeBits > 0) {
        output_ << "    sub rsp, " << paramsSizeBits / 8 << "\n";  // Allocate space for parameters
    }

    int currentParamOffset = 16;  // [rbp] is the saved rbp, [rbp + 8] is the return address
    for (const auto& param : std::views::reverse(funcDef.parameters_)) {
        output_ << "    mov rax, [rbp + " << currentParamOffset << "]\n";
        write_to_variable_from_rax(param->identifier_->name_);
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
