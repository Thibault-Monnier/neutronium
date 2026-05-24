#pragma once

#include <cstdint>
#include <memory>
#include <span>
#include <vector>

#include "Type.hpp"
#include "lib/PolymorphicArenaAllocator.hpp"
#include "lib/SpecializedArenaAllocator.hpp"

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
    /// Calculates the address: base + index * elementSize. The first operand is a pointer to the
    /// base, and the second operand is the index. Returns the calculated address as a pointer.
    GEP,

    /// Copies a block of memory from a source address to a destination address. The first operand
    /// is the destination address, the second operand is the source address, and the third operand
    /// is the number of bytes to copy.
    MEMCPY,

    /// If there is one operand, unconditionally jumps to the basic block in that operand. If there
    /// are three operands, jumps to the basic block in the second operand if the first operand is
    /// true, and to the basic block in the third operand otherwise.
    BR,

    /// Calls the function in the first operand with the rest of the operands as arguments.
    CALL,
    /// Returns the first operand from the current function if it isn't void or returns nothing if
    /// the function is void.
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
   public:
    enum class Kind : uint8_t {
        INTEGER_CONSTANT,
        ARGUMENT,
        INSTRUCTION,
        BASIC_BLOCK,
        FUNCTION,
    };

   private:
    const Type* type_;
    const Kind kind_;
    uint32_t id_ = 0;

   protected:
    explicit Value(const Type& type, const Kind kind) : type_(&type), kind_(kind) {}

    Value(Value&&) = default;

   public:
    Value(const Value&) = delete;
    Value& operator=(const Value&) = delete;
    Value& operator=(Value&&) = delete;

    template <class T>
        requires std::derived_from<T, Value>
    [[nodiscard]] bool isa() const {
        return T::classof(*this);
    }

    /** Returns a pointer to the value if it is of type T, or nullptr otherwise. */
    template <class T>
        requires std::derived_from<T, Value>
    [[nodiscard]] T* dynCast() {
        return isa<T>() ? static_cast<T*>(this) : nullptr;
    }
    /** Returns a pointer to the value if it is of type T, or nullptr otherwise. */
    template <class T>
        requires std::derived_from<T, Value>
    [[nodiscard]] const T* dynCast() const {
        return isa<T>() ? static_cast<const T*>(this) : nullptr;
    }

    static bool classof([[maybe_unused]] const Value& value) {
        assert(false && "classof should be implemented in derived classes");
        std::unreachable();
    }

   public:
    [[nodiscard]] const Type& getType() const { return *type_; }
    [[nodiscard]] Kind getKind() const { return kind_; }
    [[nodiscard]] uint32_t getID() const { return id_; }

    void setID(const uint32_t id) { id_ = id; }
};

class ConstantValue : public Value {
    using Value::Value;

   public:
    static bool classof(const Value& value) { return value.getKind() == Kind::INTEGER_CONSTANT; }
};

class IntegerConstant : public ConstantValue {
    int64_t value_;

   public:
    explicit IntegerConstant(const Type& type, const int64_t value)
        : ConstantValue(type, Kind::INTEGER_CONSTANT), value_(value) {
        assert(type.isInteger());
    }

    [[nodiscard]] int64_t getValue() const { return value_; }

    static bool classof(const Value& value) { return value.getKind() == Kind::INTEGER_CONSTANT; }
};

class Argument : public Value {
   public:
    explicit Argument(const Type& type) : Value(type, Kind::ARGUMENT) {}

    static bool classof(const Value& value) { return value.getKind() == Kind::ARGUMENT; }
};

class Instruction : public Value {
    const OpCode opcode_;
    const std::span<Value*> operands_;

    const Instruction* next_ = nullptr;

   public:
    explicit Instruction(const OpCode opcode, const Type& type, const std::span<Value*> operands)
        : Value(type, Kind::INSTRUCTION), opcode_(opcode), operands_(operands) {}

    // Forbid these to avoid accidental local references bugs.
    template <typename T, typename Alloc>
    Instruction(OpCode, const Type&, const std::vector<T, Alloc>&) = delete;
    template <typename T, size_t N>
    Instruction(OpCode, const Type&, const std::array<T, N>&) = delete;

    void setNext(const Instruction& next) { next_ = &next; }
    [[nodiscard]] const Instruction* getNext() const { return next_; }

    [[nodiscard]] OpCode getOpcode() const { return opcode_; }
    [[nodiscard]] std::span<Value*> getOperands() const { return operands_; }

    static bool classof(const Value& value) { return value.getKind() == Kind::INSTRUCTION; }
};

class BasicBlock : public Value {
    Instruction* firstInstruction_ = nullptr;
    Instruction* lastInstruction_ = nullptr;

    const BasicBlock* next_ = nullptr;

   public:
    explicit BasicBlock(const Type& type) : Value(type, Kind::BASIC_BLOCK) {}

    void addInstruction(Instruction& instr) {
        if (!firstInstruction_)
            firstInstruction_ = &instr;
        else
            lastInstruction_->setNext(instr);

        lastInstruction_ = &instr;
    }

    void setNext(const BasicBlock& next) { next_ = &next; }
    [[nodiscard]] const BasicBlock* getNext() const { return next_; }

    [[nodiscard]] Instruction* getFirstInstruction() const { return firstInstruction_; }

    static bool classof(const Value& value) { return value.getKind() == Kind::BASIC_BLOCK; }
};

class Function : public Value {
    std::string_view name_;

    const std::span<Argument*> arguments_;

    BasicBlock* firstBlock_ = nullptr;
    BasicBlock* lastBlock_ = nullptr;

    bool isExported_;
    bool isExternal_;

   public:
    explicit Function(const std::string_view name, const std::span<Argument*> arguments,
                      const Type& returnType, const bool isExported, const bool isExternal)
        : Value(returnType, Kind::FUNCTION),
          name_(name),
          arguments_(arguments),
          isExported_(isExported),
          isExternal_(isExternal) {}

    void addBlock(BasicBlock& bb) {
        if (!firstBlock_)
            firstBlock_ = &bb;
        else
            lastBlock_->setNext(bb);

        lastBlock_ = &bb;
    }

    [[nodiscard]] std::string_view getName() const { return name_; }

    [[nodiscard]] std::span<Argument*> getArguments() const { return arguments_; }

    [[nodiscard]] const BasicBlock* getFirstBasicBlock() const { return firstBlock_; }

    [[nodiscard]] bool isExported() const { return isExported_; }
    [[nodiscard]] bool isExternal() const { return isExternal_; }

    static bool classof(const Value& value) { return value.getKind() == Kind::FUNCTION; }
};

class Module {
    std::vector<Function*> functions_;

    neutro::PolymorphicArenaAllocator values_;
    neutro::SpecializedArenaAllocator<Type> types_;

    uint32_t valuesCount_ = 0;

   public:
    Function& addFunction(Function&& func) {
        Function* inserted = &registerValue(std::move(func));
        functions_.push_back(inserted);
        return *inserted;
    }

    template <class T>
    T& registerValue(T& value) = delete;  // Forbid lvalues to avoid a copy

    template <class T>
        requires std::derived_from<T, Value>
    T& registerValue(T&& value) {
        T* ptr = values_.insert(std::forward<T>(value));
        ptr->setID(valuesCount_++);
        return *ptr;
    }

    const Type& registerType(Type type) {
        for (size_t i = 0; i < types_.count(); ++i) {
            const Type& elem = types_.at(i);
            if (type == elem) {
                return elem;
            }
        }
        const uint32_t idx = types_.insert(std::move(type));
        return types_.at(idx);
    }

    [[nodiscard]] const std::vector<Function*>& getFunctions() const { return functions_; }
    [[nodiscard]] uint32_t getValuesCount() const { return valuesCount_; }
};

}  // namespace IR
