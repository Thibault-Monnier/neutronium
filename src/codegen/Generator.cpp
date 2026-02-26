#include "Generator.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "driver/Cli.hpp"
#include "lib/FastStringStream.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"

using namespace CodeGen;

neutro::FastStringStream Generator::generate() {
    output_ << ".intel_syntax noprefix\n\n";

    // Declare external functions
    for (const auto& externalFuncDecl : compilationUnit_.externalFunctions_) {
        output_ << ".extern " << functionNameWithPrefix(externalFuncDecl->identifier_->name_)
                << "\n";
    }

    if (!compilationUnit_.externalFunctions_.empty()) output_ << "\n";

    // Write the header
    output_ << ".text\n";

    if (targetType_ == TargetType::EXECUTABLE) {
        output_ << ".globl _start\n";
        output_ << ".type _start, @function\n";
        output_ << "_start:\n";
        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";
        output_ << "    call " << functionNameWithPrefix("main") << "\n";
        generateExit("0");
    }

    for (const auto& funcDef : compilationUnit_.functions_) {
        generateFunctionDefinition(*funcDef);
    }

    return std::move(output_);
}

std::string Generator::stackOffsetMemoryOperand(const uint32_t offset) {
    return "[rbp - " + std::to_string(offset) + "]";
}

std::string Generator::stackTopMemoryOperand() const {
    return stackOffsetMemoryOperand(currentSpillStackOffset_);
}

std::string Generator::getVariableStackMemoryOperand(const std::string_view name) const {
    const uint32_t offset = getVariableStackOffset(name);
    return stackOffsetMemoryOperand(offset);
}

std::string_view Generator::registerAForSize(const uint32_t size) {
    switch (size) {
        case 1:
            return "al";
        case 2:
            return "ax";
        case 4:
            return "eax";
        case 8:
            return "rax";
        default:
            std::unreachable();
    }
}

std::string Generator::label(const uint32_t labelID) { return std::format(".L{}", labelID); }

void Generator::cleanRax(const uint32_t raxValueSize) {
    switch (raxValueSize) {
        case 1:
        case 2:
            output_ << "    movzx rax, " << registerAForSize(raxValueSize) << "\n";
            break;
        default:
            break;
    }
}

void Generator::loadValueFromRax(const uint32_t size) {
    output_ << "    mov " << registerAForSize(size) << ", [rax]\n";
    cleanRax(size);
}

uint32_t Generator::push(const std::string_view reg, const uint32_t bytes) {
    currentSpillStackOffset_ += bytes;
    output_ << "    mov " << stackTopMemoryOperand() << ", " << reg << "\n";
    return currentSpillStackOffset_;
}

void Generator::pop(const std::string_view reg, const uint32_t stackOffset) {
    output_ << "    mov " << reg << ", [rbp - " << stackOffset << "]\n";
    currentSpillStackOffset_ = stackOffset - 8;  // Pop the value at stackOffset as well
}

void Generator::allocateStackSpace(const uint32_t bytes) { currentSpillStackOffset_ += bytes; }

void Generator::setStackOffset(const uint32_t bytes) { currentSpillStackOffset_ = bytes; }

void Generator::updateRsp() {
    output_ << "    lea rsp, [rbp - " << currentSpillStackOffset_ << "]\n";
}

void Generator::copyTo(const TypeID typeID, const std::string_view to) {
    const Type& type = typeManager_.getType(typeID);
    const uint32_t size = typeSize(type);
    switch (type.kind()) {
        case TypeKind::PRIMITIVE:
            output_ << "    mov " << to << ", " << registerAForSize(size) << "\n";
            break;
        case TypeKind::ARRAY:
            output_ << "    lea rbx, " << to << "\n";
            copyArrayContents("rax", "rbx", size);
            break;
        default:
            std::unreachable();
    }
}

void Generator::insertSymbol(const std::string_view name, const TypeID typeID) {
    assert(!typeManager_.getType(typeID).isVoid() && "Void type cannot be stored in a variable");

    symbolTable_.erase(name);
    currentSymbolsStackOffset_ += typeSize(typeID);

    const SymbolInfo info = {.stackOffset_ = currentSymbolsStackOffset_, .typeID_ = typeID};
    symbolTable_.emplace(name, info);
}

uint32_t Generator::getScopeFrameSize(const AST::BlockStatement& blockStmt) const {
    uint32_t frameSize = 0;
    uint32_t maxBlockFrameSize = 0;
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            const auto& varDef = *stmt->as<AST::VariableDefinition>();
            frameSize += varDefSize(varDef);
        } else if (stmt->kind_ == AST::NodeKind::BLOCK_STATEMENT) {
            const auto& innerBlock = *stmt->as<AST::BlockStatement>();
            maxBlockFrameSize = std::max(maxBlockFrameSize, getScopeFrameSize(innerBlock));
        } else if (stmt->kind_ == AST::NodeKind::IF_STATEMENT) {
            const auto& ifStmt = *stmt->as<AST::IfStatement>();
            maxBlockFrameSize = std::max(maxBlockFrameSize, getScopeFrameSize(*ifStmt.body_));
            if (const auto elseClause = ifStmt.elseClause_) {
                maxBlockFrameSize = std::max(maxBlockFrameSize, getScopeFrameSize(*elseClause));
            }
        } else if (stmt->kind_ == AST::NodeKind::WHILE_STATEMENT) {
            const auto& innerBlock = *stmt->as<AST::WhileStatement>()->body_;
            maxBlockFrameSize = std::max(maxBlockFrameSize, getScopeFrameSize(innerBlock));
        }
    }

    frameSize += maxBlockFrameSize;
    return frameSize;
}

void Generator::enterScope(const AST::BlockStatement& blockStmt) {
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            const auto& varDef = *stmt->as<AST::VariableDefinition>();
            insertSymbol(varDef.identifier_->name_, varDef.typeID_);
        }
    }
}

void Generator::writeToVariableFromRax(const std::string_view name) {
    copyTo(symbolTable_.at(name).typeID_, getVariableStackMemoryOperand(name));
}

void Generator::moveVariableToRax(const std::string_view name) {
    const uint32_t size = getVariableSize(name);
    const uint32_t stackOffset = getVariableStackOffset(name);
    output_ << "    mov " << registerAForSize(size) << ", [rbp - " << stackOffset << "]\n";
    cleanRax(size);
}

void Generator::moveNumberLitToRax(const AST::NumberLiteral& numberLit) {
    const uint32_t size = exprSize(numberLit);
    output_ << "    mov " << registerAForSize(size) << ", " << numberLit.value_ << "\n";
    cleanRax(size);
}

void Generator::moveBooleanLitToRax(const AST::BooleanLiteral& booleanLit) {
    const uint32_t size = exprSize(booleanLit);
    output_ << "    mov " << registerAForSize(size) << ", " << (booleanLit.value() ? "1" : "0")
            << "\n";
    cleanRax(size);
}

void Generator::evaluatePlaceExpressionAddressToRax(const AST::Expression& place) {
    if (place.kind_ == AST::NodeKind::IDENTIFIER) {
        const auto& identifier = *place.as<const AST::Identifier>();
        output_ << "    mov rax, rbp\n";
        output_ << "    sub rax, " << getVariableStackOffset(identifier.name_) << "\n";

    } else if (place.kind_ == AST::NodeKind::ARRAY_ACCESS) {
        const auto& arrayAccess = *place.as<const AST::ArrayAccess>();

        evaluateExpressionToRax(*arrayAccess.base_);
        const uint32_t loc = push("rax");
        evaluateExpressionToRax(*arrayAccess.index_);
        pop("rbx", loc);

        const uint32_t size = exprSize(arrayAccess);
        output_ << "    imul rax, " << size << "\n";  // Multiply index by element size
        output_ << "    add rax, rbx\n";              // Add the base address

    } else {
        std::unreachable();
    }
}

void Generator::generateArrayLit(const AST::ArrayLiteral& arrayLit,
                                 const uint32_t destinationStackOffset) {
    const std::string destinationAddress = stackOffsetMemoryOperand(destinationStackOffset);
    output_ << "    mov rbx, " << destinationAddress << "\n";

    const size_t elementSize = exprSize(arrayLit) / arrayLit.elements_.size();
    for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
        if (i != 0) output_ << "    add rbx, " << elementSize << "\n";
        const uint32_t loc = push("rbx");
        generateExpression(*arrayLit.elements_[i], currentSpillStackOffset_);
        pop("rbx", loc);
    }

    output_ << "    mov rax, " << destinationAddress << "\n";
}

void Generator::generateRepeatArrayLit(const AST::RepeatArrayLiteral& repeatArrayLit,
                                       const uint32_t destinationStackOffset) {
    generateExpression(*repeatArrayLit.element_, std::nullopt);
    const auto valueLoc = push("rax");

    const std::string destinationAddress = stackOffsetMemoryOperand(destinationStackOffset);
    output_ << "    mov rbx, " << destinationAddress << "\n";

    const size_t elementSize = exprSize(*repeatArrayLit.element_);
    assert(repeatArrayLit.count_->value_ > 0 && "Repeat array literal count should be > 0");
    for (size_t i = 0; i < static_cast<size_t>(repeatArrayLit.count_->value_); ++i) {
        if (i != 0) output_ << "    add rbx, " << elementSize << "\n";

        output_ << "    mov rax, [rbp - " << valueLoc
                << "]\n";  // Load the element value from the stack

        const uint32_t loc = push("rbx");
        copyTo(repeatArrayLit.element_->typeID_, "[rbx]");
        pop("rbx", loc);
    }

    pop("rbx", valueLoc);

    output_ << "    mov rax, " << destinationAddress << "\n";
}

void Generator::allocateAndGenerateArrayLiteral(const AST::Expression& arrayLit) {
    const AST::NodeKind kind = arrayLit.kind_;
    assert(kind == AST::NodeKind::ARRAY_LITERAL || kind == AST::NodeKind::REPEAT_ARRAY_LITERAL);

    allocateStackSpace(exprSize(arrayLit));
    output_ << "    lea rax, " << stackTopMemoryOperand() << "\n";
    const uint32_t loc = push("rax");

    if (kind == AST::NodeKind::ARRAY_LITERAL)
        generateArrayLit(*arrayLit.as<AST::ArrayLiteral>(), currentSpillStackOffset_);
    else if (kind == AST::NodeKind::REPEAT_ARRAY_LITERAL)
        generateRepeatArrayLit(*arrayLit.as<AST::RepeatArrayLiteral>(), currentSpillStackOffset_);
    else
        std::unreachable();

    pop("rbx", loc);
}

std::string Generator::functionNameWithPrefix(const std::string_view name) {
    return "__" + std::string(name);  // Prefix with "__" to avoid conflicts with keywords
}

void Generator::generateFunctionCall(const AST::FunctionCall& funcCall,
                                     const std::optional<uint32_t>& destinationStackOffset) {
    const uint32_t initialStackOffset = currentSpillStackOffset_;

    for (const auto& argument : funcCall.arguments_) {
        evaluateExpressionToRax(*argument);
        push("rax", FUNCTION_ARGUMENT_SIZE_BYTES);
    }

    updateRsp();

    output_ << "    call " << functionNameWithPrefix(funcCall.callee_->name_) << "\n";

    setStackOffset(initialStackOffset);

    if (!destinationStackOffset.has_value()) return;

    const Type& returnType = typeManager_.getType(funcCall.typeID_);
    const uint32_t size = typeSize(returnType);
    switch (returnType.kind()) {
        case TypeKind::PRIMITIVE: {
            const std::string destinationAddress =
                stackOffsetMemoryOperand(destinationStackOffset.value());
            cleanRax(size);
            output_ << "    mov rbx, " << destinationAddress << "\n";
            output_ << "    mov [rbx], " << registerAForSize(size) << "\n";
            break;
        }
        case TypeKind::ARRAY:
            copyArrayContents("rax", destinationStackOffset.value(), size);
            break;
        default:
            std::unreachable();
    }
}

void Generator::allocateAndGenerateFunctionCall(const AST::FunctionCall& funcCall) {
    assert(typeManager_.getType(funcCall.typeID_).kind() == TypeKind::ARRAY &&
           "Only function calls returning arrays require allocation");
    allocateStackSpace(exprSize(funcCall));
    output_ << "    lea rax, " << stackTopMemoryOperand() << "\n";
    const uint32_t loc = push("rax");
    generateFunctionCall(funcCall, currentSpillStackOffset_);
    pop("rbx", loc);
}

void Generator::evaluateUnaryExpressionToRax(const AST::UnaryExpression& unaryExpr) {
    evaluateExpressionToRax(*unaryExpr.operand_);

    const uint32_t operandSize = exprSize(*unaryExpr.operand_);
    const std::string_view reg = registerAForSize(operandSize);
    if (unaryExpr.operator_ == AST::Operator::SUBTRACT) {
        output_ << "    neg " << reg << "\n";
    } else if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
        output_ << "    xor " << reg << ", 1\n";
    }
}

void Generator::applyArithmeticOperatorToRax(const AST::Operator op, const std::string& other) {
    if (op == AST::Operator::DIVIDE) {
        output_ << "    cqo\n";
        output_ << "    idiv " << other << "\n";
    } else {
        std::string_view prefix;
        if (op == AST::Operator::ADD)
            prefix = "add";
        else if (op == AST::Operator::SUBTRACT)
            prefix = "sub";
        else if (op == AST::Operator::MULTIPLY)
            prefix = "imul";
        else
            std::unreachable();

        output_ << "    " << prefix << " rax, " << other << "\n";
    }
}

void Generator::evaluateBinaryExpressionToRax(const AST::BinaryExpression& binaryExpr) {
    // First evaluate right side to prevent an additional move in case of division, because the
    // numerator has to be in rax
    evaluateExpressionToRax(*binaryExpr.right_);
    const uint32_t loc = push("rax");

    evaluateExpressionToRax(*binaryExpr.left_);

    pop("rbx", loc);

    const AST::Operator op = binaryExpr.operator_;

    if (AST::isArithmeticOperator(op)) {
        applyArithmeticOperatorToRax(op, "rbx");
    } else if (AST::isComparisonOperator(op)) {
        std::string_view suffix;
        if (op == AST::Operator::EQUALS)
            suffix = "e";
        else if (op == AST::Operator::NOT_EQUALS)
            suffix = "ne";
        else if (op == AST::Operator::LESS_THAN)
            suffix = "l";
        else if (op == AST::Operator::LESS_THAN_OR_EQUAL)
            suffix = "le";
        else if (op == AST::Operator::GREATER_THAN)
            suffix = "g";
        else if (op == AST::Operator::GREATER_THAN_OR_EQUAL)
            suffix = "ge";
        else
            std::unreachable();

        output_ << "    cmp rax, rbx\n";
        output_ << "    set" << suffix << " al\n";
        output_ << "    movzx rax, al\n";
    } else if (AST::isLogicalOperator(op)) {
        // TODO: Make this short-circuiting
        if (op == AST::Operator::LOGICAL_AND) {
            output_ << "    and rax, rbx\n";
        } else if (op == AST::Operator::LOGICAL_OR) {
            output_ << "    or rax, rbx\n";
        } else {
            std::unreachable();
        }
    } else {
        std::unreachable();
    }
}

void Generator::generatePrimitiveExpression(const AST::Expression& expr,
                                            const std::optional<uint32_t>& destinationStackOffset) {
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL: {
            const auto& numberLit = *expr.as<AST::NumberLiteral>();
            moveNumberLitToRax(numberLit);
            break;
        }
        case AST::NodeKind::BOOLEAN_LITERAL: {
            const auto& booleanLit = *expr.as<AST::BooleanLiteral>();
            moveBooleanLitToRax(booleanLit);
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = *expr.as<AST::Identifier>();
            moveVariableToRax(identifier.name_);
            break;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<AST::ArrayAccess>();
            evaluatePlaceExpressionAddressToRax(arrayAccess);
            loadValueFromRax(exprSize(expr));
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            generateFunctionCall(funcCall, destinationStackOffset);
            return;
        }
        case AST::NodeKind::UNARY_EXPRESSION: {
            const auto& unaryExpr = *expr.as<AST::UnaryExpression>();
            evaluateUnaryExpressionToRax(unaryExpr);
            break;
        }
        case AST::NodeKind::BINARY_EXPRESSION: {
            const auto& binaryExpr = *expr.as<AST::BinaryExpression>();
            evaluateBinaryExpressionToRax(binaryExpr);
            break;
        }
        default:
            std::unreachable();
    }

    if (destinationStackOffset.has_value()) {
        const uint32_t size = exprSize(expr);
        const std::string destinationAddress =
            stackOffsetMemoryOperand(destinationStackOffset.value());
        output_ << "    mov rbx, " << destinationAddress << "\n";
        output_ << "    mov [rbx], " << registerAForSize(size) << "\n";
    }
}

void Generator::generateArrayExpression(const AST::Expression& expr,
                                        const std::optional<uint32_t>& destinationStackOffset) {
    switch (expr.kind_) {
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLit = *expr.as<AST::ArrayLiteral>();
            if (destinationStackOffset.has_value())
                generateArrayLit(arrayLit, destinationStackOffset.value());
            else
                allocateAndGenerateArrayLiteral(arrayLit);
            break;
        }
        case AST::NodeKind::REPEAT_ARRAY_LITERAL: {
            const auto& repeatArrayLit = *expr.as<AST::RepeatArrayLiteral>();
            if (destinationStackOffset.has_value())
                generateRepeatArrayLit(repeatArrayLit, destinationStackOffset.value());
            else
                allocateAndGenerateArrayLiteral(repeatArrayLit);
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            if (destinationStackOffset.has_value())
                generateFunctionCall(funcCall, destinationStackOffset);
            else
                allocateAndGenerateFunctionCall(funcCall);
            break;
        }
        default:
            evaluatePlaceExpressionAddressToRax(expr);
            if (destinationStackOffset.has_value())
                copyArrayContents("rax", destinationStackOffset.value(), exprSize(expr));
            break;
    }
}

void Generator::evaluateExpressionToRax(const AST::Expression& expr) {
    generateExpression(expr, std::nullopt);
}

void Generator::generateExpression(const AST::Expression& expr,
                                   const std::optional<uint32_t>& destinationStackOffset) {
    const Type& exprType = typeManager_.getType(expr.typeID_);
    switch (exprType.kind()) {
        case TypeKind::PRIMITIVE:
            generatePrimitiveExpression(expr, destinationStackOffset);
            break;

        case TypeKind::ARRAY:
            generateArrayExpression(expr, destinationStackOffset);
            break;

        default:
            std::unreachable();
    }
}

void Generator::copyArrayContents(const std::string_view sourceAddress,
                                  const std::string_view destinationAddress,
                                  const uint32_t arraySize) {
    output_ << "    mov rsi, " << sourceAddress << "\n";
    output_ << "    mov rdi, " << destinationAddress << "\n";
    output_ << "    mov rcx, " << arraySize << "\n";
    output_ << "    rep movsb\n";
}

void Generator::copyArrayContents(const std::string_view sourceAddress,
                                  const uint32_t destinationStackOffset, const uint32_t arraySize) {
    const std::string destinationAddress = stackOffsetMemoryOperand(destinationStackOffset);
    copyArrayContents(sourceAddress, destinationAddress, arraySize);
}

[[nodiscard]] int Generator::generateCondition(const AST::Expression& condition) {
    evaluateExpressionToRax(condition);
    output_ << "    cmp rax, 0\n";
    output_ << "    je " << label(labelsCount_) << "\n";
    return labelsCount_++;
}

void Generator::generateVariableDefinition(const AST::VariableDefinition& varDecl) {
    evaluatePlaceExpressionAddressToRax(*varDecl.identifier_);
    const uint32_t loc = push("rax");
    generateExpression(*varDecl.value_, currentSpillStackOffset_);
    pop("rbx", loc);
}

void Generator::generateVariableAssignment(const AST::Assignment& assignment) {
    const auto& place = assignment.place_;
    const auto& value = assignment.value_;

    evaluatePlaceExpressionAddressToRax(*place);
    const uint32_t loc = push("rax");

    if (assignment.operator_ == AST::Operator::ASSIGN) {
        generateExpression(*value, currentSpillStackOffset_);
        pop("rcx", loc);
    } else {
        const uint32_t size = exprSize(*place);
        assert(size <= 64 && "Compound assignment must have <= 64-bit size");

        AST::Operator op = {};
        if (assignment.operator_ == AST::Operator::ADD_ASSIGN)
            op = AST::Operator::ADD;
        else if (assignment.operator_ == AST::Operator::SUBTRACT_ASSIGN)
            op = AST::Operator::SUBTRACT;
        else if (assignment.operator_ == AST::Operator::MULTIPLY_ASSIGN)
            op = AST::Operator::MULTIPLY;
        else if (assignment.operator_ == AST::Operator::DIVIDE_ASSIGN)
            op = AST::Operator::DIVIDE;
        else
            std::unreachable();

        evaluateExpressionToRax(*value);

        output_ << "    mov rbx, rax\n";
        pop("rcx", loc);
        output_ << "    mov rax, [rcx]\n";

        applyArithmeticOperatorToRax(op, "rbx");
        output_ << "    mov [rcx], " << registerAForSize(size) << "\n";
    }
}

void Generator::generateExpressionStmt(const AST::ExpressionStatement& exprStmt) {
    const uint32_t initialStackOffset = currentSpillStackOffset_;
    evaluateExpressionToRax(*exprStmt.expression_);
    setStackOffset(initialStackOffset);
}

void Generator::generateIfStmt(const AST::IfStatement& ifStmt) {
    const std::string elseLabel = label(generateCondition(*ifStmt.condition_));
    const std::string endifLabel = ifStmt.elseClause_ ? label(labelsCount_++) : elseLabel;

    generateStmt(*ifStmt.body_);

    if (ifStmt.elseClause_) {
        output_ << "    jmp " << endifLabel << "\n";

        output_ << elseLabel << ":\n";
        generateStmt(*ifStmt.elseClause_);
    }

    output_ << endifLabel << ":\n";
}

void Generator::generateWhileStmt(const AST::WhileStatement& whileStmt) {
    const std::string whileLabel = label(labelsCount_++);
    innerLoopStartLabel_ = whileLabel;
    output_ << whileLabel << ":\n";

    const std::string endwhileLabel = label(generateCondition(*whileStmt.condition_));
    innerLoopEndLabel_ = endwhileLabel;

    generateStmt(*whileStmt.body_);
    output_ << "    jmp " << whileLabel << "\n";
    output_ << endwhileLabel << ":\n";
}

void Generator::generateBreakStatement() { output_ << "    jmp " << innerLoopEndLabel_ << "\n"; }

void Generator::generateContinueStatement() {
    output_ << "    jmp " << innerLoopStartLabel_ << "\n";
}

void Generator::generateReturnStatement(const AST::ReturnStatement& returnStmt) {
    if (returnStmt.returnValue_) evaluateExpressionToRax(*returnStmt.returnValue_);
    output_ << "    leave\n";
    output_ << "    ret\n";
}

void Generator::generateExit(const std::string& source) {
    output_ << "    mov rdi, " << source << "\n";  // exit code
    output_ << "    mov rax, 60\n";                // syscall: exit
    output_ << "    syscall\n";
}

void Generator::generateExit(const AST::ExitStatement& exitStmt) {
    evaluateExpressionToRax(*exitStmt.exitCode_);
    generateExit("rax");
}

void Generator::generateStmt(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = *stmt.as<AST::VariableDefinition>();
            generateVariableDefinition(varDecl);
            break;
        }
        case AST::NodeKind::ASSIGNMENT: {
            const auto& assignment = *stmt.as<AST::Assignment>();
            generateVariableAssignment(assignment);
            break;
        }
        case AST::NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = *stmt.as<AST::ExpressionStatement>();
            generateExpressionStmt(exprStmt);
            break;
        }
        case AST::NodeKind::IF_STATEMENT: {
            const auto& ifStmt = *stmt.as<AST::IfStatement>();
            generateIfStmt(ifStmt);
            break;
        }
        case AST::NodeKind::WHILE_STATEMENT: {
            const auto& whileStmt = *stmt.as<AST::WhileStatement>();
            generateWhileStmt(whileStmt);
            break;
        }
        case AST::NodeKind::BREAK_STATEMENT:
            generateBreakStatement();
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            generateContinueStatement();
            break;
        case AST::NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = *stmt.as<AST::ReturnStatement>();
            generateReturnStatement(returnStmt);
            break;
        }
        case AST::NodeKind::EXIT_STATEMENT: {
            const auto& exitStmt = *stmt.as<AST::ExitStatement>();
            generateExit(exitStmt);
            break;
        }
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = *stmt.as<AST::BlockStatement>();
            enterScope(blockStmt);
            for (const auto& innerStmt : blockStmt.body_) {
                generateStmt(*innerStmt);
            }
            break;
        }
        default:
            std::unreachable();
    }
}

void Generator::generateFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    output_ << "\n";

    const std::string& funcName = functionNameWithPrefix(funcDef.identifier_->name_);
    if (funcDef.isExported()) {
        output_ << ".globl " << funcName << "\n";
        output_ << ".type " << funcName << ", @function\n";
    }

    output_ << funcName << ":\n";

    currentSymbolsStackOffset_ = 0;
    currentSpillStackOffset_ = 0;

    output_ << "    push rbp\n";        // Save the old base pointer
    output_ << "    mov rbp, rsp\n\n";  // Set the new base pointer

    for (const auto& param : funcDef.parameters_) {
        insertSymbol(param->identifier_->name_, param->typeID_);
    }

    uint32_t paramsSize = 0;
    for (const auto& param : funcDef.parameters_)
        paramsSize += getVariableSize(param->identifier_->name_);

    if (paramsSize > 0) {
        output_ << "    sub rsp, " << paramsSize << "\n";  // Allocate space for parameters
    }

    int currentParamOffset = 16;  // [rbp] is the saved rbp, [rbp + 8] is the return address
    for (const auto& param : std::views::reverse(funcDef.parameters_)) {
        output_ << "    mov rax, [rbp + " << currentParamOffset << "]\n";
        writeToVariableFromRax(param->identifier_->name_);
        currentParamOffset += FUNCTION_ARGUMENT_SIZE_BYTES;
    }

    if (funcDef.parameters_.size() > 0) {
        output_ << "\n";
    }

    // Reserve stack space
    const uint32_t frameSize = getScopeFrameSize(*funcDef.body_);
    currentSpillStackOffset_ = currentSymbolsStackOffset_ + frameSize;  // Parameters + variables

    generateStmt(*funcDef.body_);

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
