#pragma once

#include <unordered_map>
#include <vector>

#include "ir/core/IR.hpp"

namespace IR {

/// Exposes helpers to build the IR.
class Builder {
    Module module_;

    std::unordered_map<std::string_view, Function&> functionTable_;
    Function* currentFunction_;
    BasicBlock* currentBlock_;

   public:
    Builder() = default;

    const Type& registerType(const Type type) { return module_.registerType(type); }

    Function& beginFunction(std::string_view name, std::vector<const Type*>&& parameterTypes,
                            const Type& returnType);

    Value& createIntegerConstant(const Type& type, const int64_t value) {
        return registerValue(IntegerConstant{type, value});
    }
    Value& createBooleanConstant(const bool value) {
        return registerValue(IntegerConstant{boolType(), value ? 1 : 0});
    }

    Value& createAddInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::ADD); }
    Value& createSubInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::SUB); }
    Value& createMulInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::MUL); }
    Value& createDivInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::DIV); }

    Value& createAndInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::AND); }
    Value& createOrInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::OR); }
    Value& createXorInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::XOR); }

    Value& createEqInstr(Value& a, Value& b) { return createComparisonExpr(a, b, OpCode::EQ); }
    Value& createNotEqInstr(Value& a, Value& b) { return createNotInstr(createEqInstr(a, b)); }
    Value& createLtInstr(Value& a, Value& b) { return createComparisonExpr(a, b, OpCode::LT); }
    Value& createLteInstr(Value& a, Value& b) { return createComparisonExpr(a, b, OpCode::LTE); }
    Value& createGtInstr(Value& a, Value& b) { return createComparisonExpr(b, a, OpCode::LT); }
    Value& createGteInstr(Value& a, Value& b) { return createComparisonExpr(b, a, OpCode::LTE); }

    Value& createNegInstr(Value& operand);
    Value& createNotInstr(Value& operand);

    Value& createAllocaInstr(const Type& elementType, uint32_t nbElements);
    Value& createAllocaInstr(const Type& type) { return createAllocaInstr(type, 1); }

    Value& createStoreInstr(Value& location, Value& value);
    Value& createLoadInstr(Value& location);

    Value& createGetElementPtrInstr(Value& val, Value& index);

    Value& createConditionalBranchInstr(Value& condition, BasicBlock& trueBlock,
                                        BasicBlock& falseBlock);
    Value& createUnconditionalBranchInstr(BasicBlock& targetBlock);

    Value& createCallInstr(std::string_view calleeName, std::vector<Value*>&& arguments);

    Value& createRetInstr(Value& value);
    Value& createRetInstr();

    Value& createSyscallInstr(int64_t syscallNumber, std::vector<Value*>&& arguments);

    [[nodiscard]] BasicBlock& createBasicBlock() { return currentFunction_->newBlock(voidType()); }
    void setInsertionPoint(BasicBlock& block) { currentBlock_ = &block; }

   private:
    /// Registers a value in the module and returns a reference to it.
    template <class T>
        requires std::derived_from<T, Value>
    T& registerValue(T&& value) {
        return module_.registerValue(std::forward<T>(value));
    }

    /// Adds an instruction to the end of currentBlock_ and returns a reference to it.
    Value& addInstr(Instruction&& instr) {
        auto& stored = static_cast<Instruction&>(registerValue(std::move(instr)));
        currentBlock_->addInstruction(stored);
        return stored;
    }

    Value& createArithmeticExpr(Value& a, Value& b, OpCode opCode);
    Value& createComparisonExpr(Value& a, Value& b, OpCode opCode);

    const Type& intType(const uint32_t sizeBits) {
        return module_.registerType(Type::intType(sizeBits));
    }
    const Type& boolType() { return module_.registerType(Type::boolean()); }
    const Type& voidType() { return module_.registerType(Type::voidType()); }
    const Type& ptrType(const Type& pointeeType) {
        return module_.registerType(Type::pointer(&pointeeType));
    }
};

}  // namespace IR
