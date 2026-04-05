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

    /// Performs a bitwise AND on two operands.
    AND,
    /// Performs a bitwise OR on two operands.
    OR,
    /// Performs a bitwise XOR on two operands.
    XOR,

    /// Checks whether two operands are equal. Returns a boolean.
    EQ,
    /// Checks whether the first operand is less than the second. Returns a boolean.
    LT,
    /// Checks whether the first operand is less than or equal to the second. Returns a boolean.
    LTE,
    // Note: NEQ doesn't get its own opcode and is implemented using EQ and NOT.
    // Note: GT and GTE don't get their own opcodes and are implemented using LT and LTE.

    /// Allocates memory on the stack. The first operand specifies the number of elements to
    /// allocate. Returns a pointer to the beginning of the allocated memory.
    ALLOCA,
    /// Loads the first operand from memory.
    LOAD,
    /// Stores the second operand to the first operand in memory.
    STORE,
    /// Calculates the address of the i-th element in an array, where i is the second operand and
    /// the array is the first operand.
    GEP,

    /// If there is one operand, unconditionally jumps to the basic block in that operand. If there
    /// are three operands, jumps to the basic block in the second operand if the first operand is
    /// true, and to the basic block in the third operand otherwise.
    BR,

    /// Calls the function in the first operand with the rest of the operands as arguments.
    CALL,
    /// Returns the first operand from the current function.
    RET,

    /// Performs a system call. The first operand is the syscall number, and the rest of the
    /// operands are the arguments.
    SYSCALL,
};

inline bool isBinaryArithmeticOp(const OpCode op) {
    switch (op) {
        case OpCode::ADD:
        case OpCode::SUB:
        case OpCode::MUL:
        case OpCode::DIV:
        case OpCode::AND:
        case OpCode::OR:
        case OpCode::XOR:
            return true;
        default:
            return false;
    }
}

inline bool isBinaryComparisonOp(const OpCode op) {
    switch (op) {
        case OpCode::EQ:
        case OpCode::LT:
        case OpCode::LTE:
            return true;
        default:
            return false;
    }
}

inline bool isBinaryOp(const OpCode op) {
    return isBinaryArithmeticOp(op) || isBinaryComparisonOp(op);
}

class Value {
    const Type* type_;

   protected:
    explicit Value(const Type& type) : type_(&type) {}

   public:
    virtual ~Value() = default;

    [[nodiscard]] const Type& getType() const { return *type_; }
};

class ConstantValue : public Value {
    using Value::Value;
};

class IntegerConstant : public ConstantValue {
    int64_t value_;

   public:
    explicit IntegerConstant(const Type& type, const int64_t value)
        : ConstantValue(type), value_(value) {
        assert(type.isInteger());
    }

    [[nodiscard]] int64_t getValue() const { return value_; }
};

class Argument : public Value {
   public:
    explicit Argument(const Type& type) : Value(type) {}
};

class Instruction : public Value {
    const OpCode opcode_;
    std::vector<Value*> operands_;

   public:
    explicit Instruction(const OpCode opcode, const Type& type, std::vector<Value*>&& operands)
        : Value(type), opcode_(opcode), operands_(std::move(operands)) {}

    [[nodiscard]] OpCode getOpcode() const { return opcode_; }
    [[nodiscard]] const std::vector<Value*>& getOperands() const { return operands_; }
};

class BasicBlock : public Value {
    std::vector<Instruction*> instructions_;

   public:
    explicit BasicBlock(const Type& type) : Value(type) {}

    void addInstruction(Instruction& instr) { instructions_.push_back(&instr); }

    [[nodiscard]] const std::vector<Instruction*>& getInstructions() const { return instructions_; }
};

class Function : public Value {
    std::string_view name_;
    std::vector<Argument*> arguments_;
    std::vector<std::unique_ptr<BasicBlock>> basicBlocks_;

    bool isExported_;
    bool isExternal_;

   public:
    explicit Function(const std::string_view name, std::vector<Argument*>&& arguments,
                      const Type& returnType, const bool isExported, const bool isExternal)
        : Value(returnType),
          name_(name),
          arguments_(std::move(arguments)),
          isExported_(isExported),
          isExternal_(isExternal) {}

    BasicBlock& newBlock(const Type& voidTypeInstance) {
        return *basicBlocks_.emplace_back(std::make_unique<BasicBlock>(voidTypeInstance));
    }

    [[nodiscard]] std::string_view getName() const { return name_; }

    [[nodiscard]] const std::vector<Argument*>& getArguments() const { return arguments_; }

    [[nodiscard]] const std::vector<std::unique_ptr<BasicBlock>>& getBasicBlocks() const {
        return basicBlocks_;
    }

    [[nodiscard]] bool isExported() const { return isExported_; }
    [[nodiscard]] bool isExternal() const { return isExternal_; }
};

class Module {
    std::vector<std::unique_ptr<Function>> functions_;

    std::vector<std::unique_ptr<Value>> values_;
    std::vector<std::unique_ptr<Type>> types_;

   public:
    Function& addFunction(Function&& func) {
        functions_.push_back(std::make_unique<Function>(std::move(func)));
        return *functions_.back();
    }

    template <class T>
    T& registerValue(T& value) = delete;  // Forbid lvalues to avoid a copy

    template <class T>
        requires std::derived_from<T, Value>
    T& registerValue(T&& value) {
        values_.push_back(std::make_unique<T>(std::forward<T>(value)));
        return static_cast<T&>(*values_.back());
    }

    const Type& registerType(const Type type) {
        types_.push_back(std::make_unique<Type>(type));
        return *types_.back();
    }

    [[nodiscard]] const std::vector<std::unique_ptr<Function>>& getFunctions() const {
        return functions_;
    }
};

}  // namespace IR
