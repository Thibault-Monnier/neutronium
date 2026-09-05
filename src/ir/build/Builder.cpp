#include "Builder.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string_view>
#include <utility>
#include <vector>

#include "ir/core/IR.hpp"
#include "ir/core/Type.hpp"

namespace IR {

Function& Builder::beginFunction(const std::string_view name, const std::span<Argument*> arguments,
                                 const Type& returnType, const bool isExported,
                                 const bool isExternal) {
    Function& func = module_.addFunction(
        Function{name, arena_.insertRange(arguments), returnType, isExported, isExternal});
    functionTable_.emplace(name, func);
    currentFunction_ = &func;

    if (!isExternal)
        setInsertionPoint(createBasicBlock());
    else
        currentBlock_ = nullptr;

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

Value& Builder::createAllocaInstr(const Type& type, const uint32_t nbElements) {
    Value& nbElementsValue = registerValue(IntegerConstant{intType(32), nbElements});
    std::array operands = {&nbElementsValue};
    return addInstr(Instruction{OpCode::ALLOCA, ptrType(type), arena_.insertRange(operands)});
}

Value& Builder::createStoreInstr(Value& location, Value& value) {
    assert(location.getType().isPointer());
    assert(value.getType().isScalar());
    assert(location.getType().getSubtype() == value.getType());
    std::array operands = {&location, &value};
    return addInstr(Instruction{OpCode::STORE, voidType(), arena_.insertRange(operands)});
}

Value& Builder::createLoadInstr(Value& location) {
    assert(location.getType().isPointer());
    std::array operands = {&location};
    return addInstr(
        Instruction{OpCode::LOAD, location.getType().getSubtype(), arena_.insertRange(operands)});
}

Value& Builder::createGetElementPtrInstr(Value& val, Value& index) {
    assert(val.getType().holdsSubtype() && val.getType().getSubtype().holdsSubtype());
    const Type& pointeeType = val.getType().getSubtype().getSubtype();
    std::array operands = {&val, &index};
    return addInstr(Instruction{OpCode::GEP, ptrType(pointeeType), arena_.insertRange(operands)});
}

Value& Builder::createMemcpyInstr(Value& dest, Value& src, Value& size) {
    assert(dest.getType().isPointer());
    assert(src.getType().isPointer());
    assert(size.getType().isInteger());
    assert(dest.getType() == src.getType());

    std::array operands = {&dest, &src, &size};
    return addInstr(Instruction{OpCode::MEMCPY, voidType(), arena_.insertRange(operands)});
}

Value& Builder::createConditionalBranchInstr(Value& condition, BasicBlock& trueBlock,
                                             BasicBlock& falseBlock) {
    std::array<Value*, 3> operands = {&condition, &trueBlock, &falseBlock};
    return addInstr(Instruction{OpCode::BR, voidType(), arena_.insertRange(operands)});
}

Value& Builder::createUnconditionalBranchInstr(BasicBlock& targetBlock) {
    std::array<Value*, 1> operands = {&targetBlock};
    return addInstr(Instruction{OpCode::BR, voidType(), arena_.insertRange(operands)});
}

Value& Builder::createCallInstr(const std::string_view calleeName,
                                const std::span<Value*> arguments) {
    Function& callee = functionTable_.at(calleeName);

    auto** mem = arena_.allocateElems<Value*>(arguments.size() + 1);
    mem[0] = &callee;
    std::ranges::copy(arguments, mem + 1);

    return addInstr(Instruction{OpCode::CALL, callee.getType(), {mem, arguments.size() + 1}});
}

Value& Builder::createRetInstr(Value& value) {
    // The return type should match the function's return type
    assert(currentFunction_->getType() == value.getType());

    std::array operands = {&value};
    return addInstr(Instruction{OpCode::RET, voidType(), arena_.insertRange(operands)});
}

Value& Builder::createRetInstr() {
    // The return type should match the function's return type
    assert(currentFunction_->getType().isVoid());

    return addInstr(Instruction{OpCode::RET, voidType(), {}});
}

Value& Builder::createSyscallInstr(const int64_t syscallNumber, const std::span<Value*> arguments) {
    assert(syscallNumber == 60 && "Only the `exit` syscall is currently supported");

    Value& syscallNumberValue = registerValue(IntegerConstant{intType(64), syscallNumber});

    auto** mem = arena_.allocateElems<Value*>(arguments.size() + 1);
    mem[0] = &syscallNumberValue;
    std::ranges::copy(arguments, mem + 1);

    return addInstr(Instruction{OpCode::SYSCALL, intType(64), {mem, arguments.size() + 1}});
}

Value& Builder::createArithmeticExpr(Value& a, Value& b, const OpCode opCode) {
    std::array operands = {&a, &b};

    const Type& type = a.getType();
    assert(b.getType() == type);
    assert(type.isInteger());

    return addInstr(Instruction{opCode, type, arena_.insertRange(operands)});
}

Value& Builder::createComparisonExpr(Value& a, Value& b, const OpCode opCode) {
    std::array operands = {&a, &b};

    assert(a.getType() == b.getType());
    assert(a.getType().isInteger());

    return addInstr(Instruction{opCode, boolType(), arena_.insertRange(operands)});
}

}  // namespace IR
