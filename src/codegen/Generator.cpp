#include "Generator.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "driver/Cli.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"

using namespace CodeGen;

std::stringstream Generator::generate() {
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

void Generator::insertSymbol(const std::string_view name, const TypeID typeID) {
    symbolTable_.erase(name);

    const Type& type = typeManager_.getType(typeID);
    assert(!type.isVoid() && "Void type cannot be stored in a variable");

    const uint32_t sizeBits = typeSizeBits(type);

    currentSymbolsStackOffset_ += sizeBits / 8;

    const SymbolInfo info = {.name_ = name,
                             .stackOffset_ = currentSymbolsStackOffset_,
                             .stackSizeBits_ = sizeBits,
                             .typeID_ = typeID};
    symbolTable_.emplace(name, info);
}

uint32_t Generator::getScopeFrameSize(const AST::BlockStatement& blockStmt) const {
    uint32_t frameSize = 0;
    uint32_t maxBlockFrameSize = 0;
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::VARIABLE_DEFINITION) {
            const auto& varDef = *stmt->as<AST::VariableDefinition>();
            frameSize += varDefSizeBits(varDef) / 8;
        } else if (stmt->kind_ == AST::NodeKind::BLOCK_STATEMENT) {
            const auto& innerBlock = *stmt->as<AST::BlockStatement>();
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

uint32_t Generator::getVariableSizeBits(const std::string_view name) const {
    return symbolTable_.at(name).stackSizeBits_;
}

uint32_t Generator::getVariableStackOffset(const std::string_view name) const {
    assert(symbolTable_.contains(name) && "Variable not found in stack offset map");

    return symbolTable_.at(name).stackOffset_;
}

std::string_view Generator::registerAForSize(const uint32_t bitSize) {
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

std::string Generator::label(const uint32_t labelID) { return std::format(".L{}", labelID); }

void Generator::cleanRax(const uint32_t raxValueSizeBits) {
    switch (raxValueSizeBits) {
        case 8:
        case 16:
            output_ << "    movzx rax, " << registerAForSize(raxValueSizeBits) << "\n";
            break;
        default:
            break;
    }
}

void Generator::loadValueFromRax(const uint32_t bitSize) {
    output_ << "    mov " << registerAForSize(bitSize) << ", [rax]\n";
    cleanRax(bitSize);
}

uint32_t Generator::push(const std::string_view reg, const uint32_t sizeBits) {
    currentSpillStackOffset_ += sizeBits / 8;
    output_ << "    mov " << stackTopMemoryOperand() << ", " << reg << "\n";
    return currentSpillStackOffset_ * 8;
}

void Generator::pop(const std::string_view reg, const uint32_t offsetBits) {
    output_ << "    mov " << reg << ", [rbp - " << (offsetBits / 8) << "]\n";
    currentSpillStackOffset_ = offsetBits / 8;
}

void Generator::allocateStackSpace(const uint32_t sizeBits) {
    currentSpillStackOffset_ += sizeBits / 8;
}

void Generator::setStackOffset(const uint32_t offsetBits) {
    currentSpillStackOffset_ = offsetBits / 8;
}

void Generator::updateRsp() {
    output_ << "    lea rsp, [rbp - " << currentSpillStackOffset_ << "]\n";
}

std::string Generator::stackTopMemoryOperand() const {
    return "[rbp - " + std::to_string(currentSpillStackOffset_) + "]";
}

void Generator::writeToVariableFromRax(const std::string_view name) {
    const Type& type = typeManager_.getType(symbolTable_.at(name).typeID_);
    const uint32_t size = getVariableSizeBits(name);
    const uint32_t stackOffset = getVariableStackOffset(name);

    switch (type.kind()) {
        case TypeKind::PRIMITIVE:
            output_ << "    mov [rbp - " << stackOffset << "], " << registerAForSize(size) << "\n";
            break;
        case TypeKind::ARRAY:
            output_ << "    mov rbx, rbp\n";
            output_ << "    sub rbx, " << stackOffset << "\n";
            copyArrayContents("rax", "rbx", size);
            break;
        default:
            std::unreachable();
    }
}

void Generator::moveVariableToRax(const std::string_view name) {
    const uint32_t sizeBits = getVariableSizeBits(name);
    const uint32_t stackOffset = getVariableStackOffset(name);
    output_ << "    mov " << registerAForSize(sizeBits) << ", [rbp - " << stackOffset << "]\n";
    cleanRax(sizeBits);
}

void Generator::moveNumberLitToRax(const AST::NumberLiteral& numberLit) {
    const uint32_t sizeBits = exprSizeBits(numberLit);
    output_ << "    mov " << registerAForSize(sizeBits) << ", " << numberLit.value_ << "\n";
    cleanRax(sizeBits);
}

void Generator::moveBooleanLitToRax(const AST::BooleanLiteral& booleanLit) {
    const uint32_t sizeBits = exprSizeBits(booleanLit);
    output_ << "    mov " << registerAForSize(sizeBits) << ", " << (booleanLit.value() ? "1" : "0")
            << "\n";
    cleanRax(sizeBits);
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

        const uint32_t sizeBits = exprSizeBits(arrayAccess);
        output_ << "    imul rax, " << (sizeBits / 8) << "\n";  // Multiply index by element size
        output_ << "    add rax, rbx\n";                        // Add the base address

    } else {
        std::unreachable();
    }
}

void Generator::generateArrayLit(const AST::ArrayLiteral& arrayLit,
                                 const std::string_view destinationAddress) {
    output_ << "    mov rbx, " << destinationAddress << "\n";

    const size_t elementSizeBits = exprSizeBits(arrayLit) / arrayLit.elements_.size();
    for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
        if (i != 0) output_ << "    add rbx, " << elementSizeBits / 8 << "\n";
        const uint32_t loc = push("rbx");
        generateExpression(*arrayLit.elements_[i], stackTopMemoryOperand());
        pop("rbx", loc);
    }

    output_ << "    mov rax, " << destinationAddress << "\n";
}

void Generator::allocateAndGenerateArrayLiteral(const AST::ArrayLiteral& arrayLit) {
    allocateStackSpace(exprSizeBits(arrayLit));
    output_ << "    lea rax, " << stackTopMemoryOperand() << "\n";
    const uint32_t loc = push("rax");
    generateArrayLit(arrayLit, stackTopMemoryOperand());
    pop("rbx", loc);
}

std::string Generator::functionNameWithPrefix(const std::string_view name) {
    return "__" + std::string(name);  // Prefix with "__" to avoid conflicts with NASM keywords
}

void Generator::generateFunctionCall(const AST::FunctionCall& funcCall,
                                     const std::optional<std::string_view>& destinationAddress) {
    const uint32_t initialStackOffsetBits = currentSpillStackOffset_ * 8;

    for (const auto& argument : funcCall.arguments_) {
        evaluateExpressionToRax(*argument);
        push("rax", FUNCTION_ARGUMENT_SIZE_BITS);
    }

    updateRsp();

    output_ << "    call " << functionNameWithPrefix(funcCall.callee_->name_) << "\n";

    setStackOffset(initialStackOffsetBits);

    if (!destinationAddress.has_value()) {
        return;
    }

    const Type& returnType = typeManager_.getType(funcCall.typeID_);
    const uint32_t sizeBits = typeSizeBits(returnType);
    switch (returnType.kind()) {
        case TypeKind::PRIMITIVE:
            cleanRax(sizeBits);
            output_ << "    mov rbx, " << destinationAddress.value() << "\n";
            output_ << "    mov [rbx], " << registerAForSize(sizeBits) << "\n";
            break;
        case TypeKind::ARRAY:
            copyArrayContents("rax", destinationAddress.value(), sizeBits);
            break;
        default:
            std::unreachable();
    }
}

void Generator::allocateAndGenerateFunctionCall(const AST::FunctionCall& funcCall) {
    assert(typeManager_.getType(funcCall.typeID_).kind() == TypeKind::ARRAY &&
           "Only function calls returning arrays require allocation");
    allocateStackSpace(exprSizeBits(funcCall));
    output_ << "    lea rax, " << stackTopMemoryOperand() << "\n";
    const uint32_t loc = push("rax");
    generateFunctionCall(funcCall, stackTopMemoryOperand());
    pop("rbx", loc);
}

void Generator::evaluateUnaryExpressionToRax(const AST::UnaryExpression& unaryExpr) {
    evaluateExpressionToRax(*unaryExpr.operand_);

    const uint32_t operandSizeBits = exprSizeBits(*unaryExpr.operand_);
    const std::string_view reg = registerAForSize(operandSizeBits);
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
        const std::unordered_map<AST::Operator, std::string> arithmeticOperatorPrefixes = {
            {AST::Operator::ADD, "add"},
            {AST::Operator::SUBTRACT, "sub"},
            {AST::Operator::MULTIPLY, "imul"},
        };
        assert(arithmeticOperatorPrefixes.contains(op) && "Unsupported arithmetic operator");
        output_ << "    " << arithmeticOperatorPrefixes.at(op) << " rax, " << other << "\n";
    }
}

void Generator::evaluateBinaryExpressionToRax(const AST::BinaryExpression& binaryExpr) {
    // First evaluate right side to prevent an additional move in case of division, because the
    // numerator has to be in rax
    evaluateExpressionToRax(*binaryExpr.right_);
    const uint32_t rightSizeBits = exprSizeBits(*binaryExpr.right_);
    cleanRax(rightSizeBits);
    const uint32_t loc = push("rax");

    evaluateExpressionToRax(*binaryExpr.left_);
    const uint32_t leftSizeBits = exprSizeBits(*binaryExpr.left_);
    cleanRax(leftSizeBits);

    pop("rbx", loc);

    const AST::Operator op = binaryExpr.operator_;

    if (AST::isArithmeticOperator(op)) {
        applyArithmeticOperatorToRax(op, "rbx");
    } else if (AST::isComparisonOperator(op)) {
        const std::unordered_map<AST::Operator, std::string_view> comparisonSuffixes = {
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

void Generator::generatePrimitiveExpression(
    const AST::Expression& expr, const std::optional<std::string_view>& destinationAddress) {
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
            loadValueFromRax(exprSizeBits(expr));
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            generateFunctionCall(funcCall, destinationAddress);
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

    if (destinationAddress.has_value()) {
        const uint32_t sizeBits = exprSizeBits(expr);
        output_ << "    mov rbx, " << destinationAddress.value() << "\n";
        output_ << "    mov [rbx], " << registerAForSize(sizeBits) << "\n";
    }
}

void Generator::generateArrayExpression(const AST::Expression& expr,
                                        const std::optional<std::string_view>& destinationAddress) {
    if (expr.kind_ == AST::NodeKind::ARRAY_LITERAL) {
        const auto& arrayLit = *expr.as<AST::ArrayLiteral>();
        if (destinationAddress.has_value()) {
            generateArrayLit(arrayLit, destinationAddress.value());
        } else {
            allocateAndGenerateArrayLiteral(arrayLit);
        }

    } else if (expr.kind_ == AST::NodeKind::FUNCTION_CALL) {
        const auto& funcCall = *expr.as<AST::FunctionCall>();
        if (destinationAddress.has_value()) {
            generateFunctionCall(funcCall, destinationAddress);
        } else {
            allocateAndGenerateFunctionCall(funcCall);
        }

    } else {
        evaluatePlaceExpressionAddressToRax(expr);
        if (destinationAddress.has_value()) {
            copyArrayContents("rax", destinationAddress.value(), exprSizeBits(expr));
        } else {
            // Do nothing: the result is already in rax
        }
    }
}

void Generator::evaluateExpressionToRax(const AST::Expression& expr) {
    const uint32_t initialStackOffsetBits = currentSpillStackOffset_ * 8;
    generateExpression(expr, std::nullopt);
    setStackOffset(initialStackOffsetBits);
}

void Generator::generateExpression(const AST::Expression& expr,
                                   const std::optional<std::string_view>& destinationAddress) {
    const Type& exprType = typeManager_.getType(expr.typeID_);
    switch (exprType.kind()) {
        case TypeKind::PRIMITIVE:
            generatePrimitiveExpression(expr, destinationAddress);
            break;

        case TypeKind::ARRAY:
            generateArrayExpression(expr, destinationAddress);
            break;

        default:
            std::unreachable();
    }
}

void Generator::copyArrayContents(const std::string_view sourceAddress,
                                  const std::string_view destinationAddress,
                                  const uint32_t arraySizeBits) {
    output_ << "    mov rsi, " << sourceAddress << "\n";
    output_ << "    mov rdi, " << destinationAddress << "\n";
    output_ << "    mov rcx, " << (arraySizeBits / 8) << "\n";
    output_ << "    rep movsb\n";
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
    generateExpression(*varDecl.value_, stackTopMemoryOperand());
    pop("rbx", loc);
}

void Generator::generateVariableAssignment(const AST::Assignment& assignment) {
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

    evaluatePlaceExpressionAddressToRax(*place);
    const uint32_t loc = push("rax");

    if (assignment.operator_ == AST::Operator::ASSIGN) {
        generateExpression(*value, stackTopMemoryOperand());
        pop("rcx", loc);
    } else {
        const uint32_t sizeBits = exprSizeBits(*place);
        assert(sizeBits <= 64 && "Compound assignment must have <= 64-bit size");

        evaluateExpressionToRax(*value);

        output_ << "    mov rbx, rax\n";
        pop("rcx", loc);
        output_ << "    mov rax, [rcx]\n";

        applyArithmeticOperatorToRax(op, "rbx");
        output_ << "    mov [rcx], " << registerAForSize(sizeBits) << "\n";
    }
}

void Generator::generateExpressionStmt(const AST::ExpressionStatement& exprStmt) {
    const auto& expr = *exprStmt.expression_;
    evaluateExpressionToRax(expr);
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

    uint32_t paramsSizeBits = 0;
    for (const auto& param : funcDef.parameters_)
        paramsSizeBits += getVariableSizeBits(param->identifier_->name_);

    if (paramsSizeBits > 0) {
        output_ << "    sub rsp, " << paramsSizeBits / 8 << "\n";  // Allocate space for parameters
    }

    int currentParamOffset = 16;  // [rbp] is the saved rbp, [rbp + 8] is the return address
    for (const auto& param : std::views::reverse(funcDef.parameters_)) {
        output_ << "    mov rax, [rbp + " << currentParamOffset << "]\n";
        writeToVariableFromRax(param->identifier_->name_);
        currentParamOffset += FUNCTION_ARGUMENT_SIZE_BITS / 8;
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
