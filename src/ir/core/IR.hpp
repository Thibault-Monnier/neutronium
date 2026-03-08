#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "Type.hpp"

namespace IR {

/// Represents an operation code for an instruction in the IR.
enum class OpCode : uint8_t {
    /// Adds two operands.
    ADD,
    /// Subtracts the second operand from the first.
    SUB,
    /// Multiplies two operands.
    MUL,
    /// Divides the first operand by the second.
    DIV,
    /// Loads the first operand from memory.
    LOAD,
    /// Stores the second operand to the first operand in memory.
    STORE,
    /// Calls the function in the first operand with the rest of the operands as arguments.
    CALL,
    /// Returns the first operand from the current function.
    RET,
    /// If there is one operand, unconditionally jumps to the basic block in that operand. If there
    /// are three operands, jumps to the basic block in the second operand if the first operand is
    /// true, and to the basic block in the third operand otherwise.
    BR
};

class Value {
    const Type type_;

   public:
    explicit Value(const Type type) : type_(type) {}
    virtual ~Value() = default;

    [[nodiscard]] const Type& getType() const { return type_; }
};

class ConstantValue : public Value {
    using Value::Value;
};

class IntegerConstant : public ConstantValue {
    int64_t value_;

    explicit IntegerConstant(const int64_t value, const Type type)
        : ConstantValue(type), value_(value) {}

   public:
    static IntegerConstant int8(int8_t value) { return IntegerConstant(value, Type::int8()); }
    static IntegerConstant int16(int16_t value) { return IntegerConstant(value, Type::int16()); }
    static IntegerConstant int32(int32_t value) { return IntegerConstant(value, Type::int32()); }
    static IntegerConstant int64(int64_t value) { return IntegerConstant(value, Type::int64()); }

    [[nodiscard]] int64_t getValue() const { return value_; }
};

class BooleanConstant : public ConstantValue {
    bool value_;

    explicit BooleanConstant(const bool value) : ConstantValue(Type::boolean()), value_(value) {}

   public:
    static BooleanConstant boolean(const bool value) { return BooleanConstant(value); }

    [[nodiscard]] bool getValue() const { return value_; }
};

class Instruction : public Value {
    const OpCode opcode_;
    std::vector<Value*> operands_;

   public:
    Instruction(const OpCode opcode, const Type type, std::vector<Value*>&& operands)
        : Value(type), opcode_(opcode), operands_(std::move(operands)) {}

    [[nodiscard]] OpCode getOpcode() const { return opcode_; }
    [[nodiscard]] const std::vector<Value*>& getOperands() const { return operands_; }
};

class BasicBlock : public Value {
    std::vector<Instruction*> instructions_;

   public:
    BasicBlock() : Value(Type::voidType()) {}

    void addInstruction(Instruction& instr) { instructions_.push_back(&instr); }
};

class Function : public Value {
    std::vector<Type> parameterTypes_;
    std::vector<BasicBlock*> basicBlocks_;

   public:
    Function(std::vector<Type>&& parameterTypes, const Type returnType)
        : Value(returnType), parameterTypes_(std::move(parameterTypes)) {}

    void newBlock() { basicBlocks_.emplace_back(); }
    void addInstruction(Instruction& instr) const { basicBlocks_.back()->addInstruction(instr); }

    [[nodiscard]] const std::vector<Type>& getParameterTypes() const { return parameterTypes_; }
    [[nodiscard]] const std::vector<BasicBlock*>& getBasicBlocks() const { return basicBlocks_; }
};

class Module {
    std::vector<Function> functions_;

    std::vector<std::unique_ptr<Value>> values_;

   public:
    Function& addFunction(Function&& func) {
        functions_.push_back(std::move(func));
        return functions_.back();
    }

    template <class T>
    T& registerValue(T& value) = delete;  // Forbid lvalues to avoid a copy

    template <class T>
        requires std::derived_from<T, Value>
    T& registerValue(T&& value) {
        values_.push_back(std::make_unique<T>(std::forward<T>(value)));
        return static_cast<T&>(*values_.back());
    }

    [[nodiscard]] const std::vector<Function>& getFunctions() const { return functions_; }
};

}  // namespace IR
