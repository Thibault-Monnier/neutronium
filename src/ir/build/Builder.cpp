#include "Builder.hpp"

#include <cassert>
#include <cstdint>
#include <string_view>
#include <utility>
#include <vector>

#include "ir/core/IR.hpp"
#include "ir/core/Type.hpp"

namespace IR {

Function& Builder::beginFunction(std::string_view name, std::vector<Argument*>&& arguments,
                                 const Type& returnType, const bool isExported,
                                 const bool isExternal) {
    Function& func = module_.addFunction(
        Function{name, std::move(arguments), returnType, isExported, isExternal});
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

Value& Builder::createAllocaInstr(const Type& type, const uint32_t nbElements) {
    Value& nbElementsValue = registerValue(IntegerConstant{intType(32), nbElements});
    std::vector<Value*> operands = {&nbElementsValue};
    return addInstr(Instruction{OpCode::ALLOCA, ptrType(type), std::move(operands)});
}

Value& Builder::createStoreInstr(Value& location, Value& value) {
    assert(location.getType().isPointer());
    assert(value.getType().isScalar());
    assert(location.getType().getSubtype() == value.getType());
    std::vector<Value*> operands = {&location, &value};
    return addInstr(Instruction{OpCode::STORE, voidType(), std::move(operands)});
}

Value& Builder::createLoadInstr(Value& location) {
    assert(location.getType().isPointer());
    std::vector<Value*> operands = {&location};
    return addInstr(
        Instruction{OpCode::LOAD, location.getType().getSubtype(), std::move(operands)});
}

Value& Builder::createGetElementPtrInstr(Value& val, Value& index) {
    assert(val.getType().holdsSubtype() && val.getType().getSubtype().holdsSubtype());
    const Type& pointeeType = val.getType().getSubtype().getSubtype();
    std::vector<Value*> operands{&val, &index};
    return addInstr(Instruction{OpCode::GEP, ptrType(pointeeType), std::move(operands)});
}

Value& Builder::createMemcpyInstr(Value& dest, Value& src, Value& size) {
    assert(dest.getType().isPointer());
    assert(src.getType().isPointer());
    assert(size.getType().isInteger());
    assert(dest.getType() == src.getType());
    std::vector<Value*> operands = {&dest, &src, &size};
    return addInstr(Instruction{OpCode::MEMCPY, voidType(), std::move(operands)});
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

Value& Builder::createSyscallInstr(const int64_t syscallNumber, std::vector<Value*>&& arguments) {
    assert(syscallNumber == 60 && "Only the `exit` syscall is currently supported");

    Value& syscallNumberValue = registerValue(IntegerConstant{intType(64), syscallNumber});
    std::vector<Value*> operands = {&syscallNumberValue};
    operands.append_range(arguments);

    return addInstr(Instruction{OpCode::SYSCALL, intType(64), std::move(operands)});
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
