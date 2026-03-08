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

    std::unordered_map<std::string_view, Value*> allocated_;

   public:
    Builder() = default;

    Value& registerValue(Value&& value) { return module_.registerValue(std::move(value)); }

    void allocate(std::string_view name, Type type);

    void beginFunction(std::string_view name, std::vector<Type>&& parameterTypes, Type returnType);

    Value& createAddInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::ADD); }
    Value& createSubInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::SUB); }
    Value& createMulInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::MUL); }
    Value& createDivInstr(Value& a, Value& b) { return createArithmeticExpr(a, b, OpCode::DIV); }

    Value& createStoreInstr(Value& location, Value& value);
    Value& createStoreInstr(const std::string_view name, Value&& value) {
        Value& location = *allocated_.at(name);
        return createStoreInstr(location, value);
    }

    Value& createLoadInstr(Value& location);
    Value& createLoadInstr(const std::string_view name) {
        Value& location = *allocated_.at(name);
        return createLoadInstr(location);
    }

    Value& createCallInstr(std::string_view calleeName, std::vector<Value*>&& arguments);

    Value& createRetInstr(Value& value);

    Value& createConditionalBranchInstr(Value& condition, BasicBlock& trueBlock,
                                        BasicBlock& falseBlock);
    Value& createUnconditionalBranchInstr(BasicBlock& targetBlock);

   private:
    /// Adds an instruction to the end of currentFunction_.
    Value& addInstr(Instruction&& instr) {
        auto& stored = static_cast<Instruction&>(registerValue(std::move(instr)));
        currentFunction_->addInstruction(stored);
        return stored;
    }

    Value& createArithmeticExpr(Value& a, Value& b, OpCode opCode);
};

}  // namespace IR
