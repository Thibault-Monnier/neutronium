#include "Builder.hpp"

#include <algorithm>
#include <cassert>
#include <ranges>

namespace IR {

Function& Builder::beginFunction(std::string_view name, std::vector<const Type*>&& parameterTypes,
                                 const Type& returnType) {
    std::vector<Value*> parameters;
    parameters.reserve(parameterTypes.size());
    for (const auto& param : parameterTypes) {
        const Type& parameterType = ptrType(*param);
        parameters.push_back(&registerValue(Value{parameterType}));
    }

    Function& func = module_.addFunction(Function{std::move(parameters), returnType});
    functionTable_.emplace(name, func);
    currentFunction_ = &func;
    setInsertionPoint(createBasicBlock());

    return *currentFunction_;
}

Value& Builder::createNegInstr(Value& operand) {
    assert(operand.getType().isInteger());
    const Type& type = operand.getType();
    Value& zero = registerValue(IntegerConstant{type, 0});
    return createSubInstr(zero, operand);
}

Value& Builder::createNotInstr(Value& operand) {
    assert(operand.getType().isBoolean());
    const Type& type = operand.getType();
    Value& one = registerValue(IntegerConstant{type, 1});
    return createXorInstr(operand, one);
}

Value& Builder::createAllocaInstr(const Type& elementType, const uint32_t nbElements) {
    Value& nbElementsValue = registerValue(IntegerConstant{intType(32), nbElements});
    std::vector<Value*> operands = {&nbElementsValue};
    return addInstr(Instruction{OpCode::ALLOCA, ptrType(elementType), std::move(operands)});
}

Value& Builder::createStoreInstr(Value& location, Value& value) {
    assert(location.getType().getKind() == Type::Kind::PTR);
    std::vector<Value*> operands = {&location, &value};
    return addInstr(Instruction{OpCode::STORE, voidType(), std::move(operands)});
}

Value& Builder::createLoadInstr(Value& location) {
    assert(location.getType().getKind() == Type::Kind::PTR);
    std::vector<Value*> operands = {&location};
    return addInstr(
        Instruction{OpCode::LOAD, location.getType().getPointeeType(), std::move(operands)});
}

Value& Builder::createGetElementPtrInstr(Value& basePtr, Value& index) {
    assert(basePtr.getType().getKind() == Type::Kind::PTR);
    const Type& elementType = basePtr.getType().getPointeeType();
    std::vector<Value*> operands{&basePtr, &index};
    return addInstr(Instruction{OpCode::GEP, ptrType(elementType), std::move(operands)});
}

Value& Builder::createCallInstr(const std::string_view calleeName,
                                std::vector<Value*>&& arguments) {
    Function& callee = functionTable_.at(calleeName);

    std::vector<Value*> operands{&callee};
    operands.append_range(arguments);

    return addInstr(Instruction{OpCode::CALL, callee.getType(), std::move(operands)});
}

Value& Builder::createRetInstr(Value& value) {
    // The return type should match the function's return type
    assert(currentFunction_->getType() == value.getType());

    std::vector<Value*> operands{&value};
    return addInstr(Instruction{OpCode::RET, voidType(), std::move(operands)});
}

Value& Builder::createRetInstr() {
    // The return type should match the function's return type
    assert(currentFunction_->getType().isVoid());

    return addInstr(Instruction{OpCode::RET, voidType(), {}});
}

Value& Builder::createConditionalBranchInstr(Value& condition, BasicBlock& trueBlock,
                                             BasicBlock& falseBlock) {
    std::vector<Value*> operands = {&condition, &trueBlock, &falseBlock};
    return addInstr(Instruction{OpCode::BR, voidType(), std::move(operands)});
}

Value& Builder::createUnconditionalBranchInstr(BasicBlock& targetBlock) {
    std::vector<Value*> operands = {&targetBlock};
    return addInstr(Instruction{OpCode::BR, voidType(), std::move(operands)});
}

Value& Builder::createArithmeticExpr(Value& a, Value& b, const OpCode opCode) {
    std::vector<Value*> operands = {&a, &b};

    const Type& type = a.getType();
    assert(b.getType() == type);
    assert(type.isInteger());

    return addInstr(Instruction{opCode, type, std::move(operands)});
}

Value& Builder::createComparisonExpr(Value& a, Value& b, const OpCode opCode) {
    std::vector<Value*> operands = {&a, &b};

    assert(a.getType() == b.getType());
    assert(a.getType().isInteger());

    return addInstr(Instruction{opCode, boolType(), std::move(operands)});
}

}  // namespace IR
