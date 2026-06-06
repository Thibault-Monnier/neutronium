#pragma once

#include <cstdint>
#include <cstdlib>
#include <deque>
#include <memory>
#include <ranges>
#include <string>
#include <string_view>

#include "Reg.hpp"
#include "driver/Cli.hpp"
#include "ir/core/IR.hpp"
#include "lib/FastStringStream.hpp"

namespace Backend {

/// Consumes IR and produces X86-64 assembly.
class CodeGen {
    static constexpr uint32_t PTR_SIZE_BITS = 64;

    const IR::Module& ir_;

    neutro::FastStringStream output_;

    const TargetType targetType_;

    /// In bits.
    uint32_t stackOffset_ = 0;

    static constexpr int32_t UNINITIALIZED_STACK_OFFSET = INT32_MAX;

    /// Maps value ID to the stack offset where it's stored. If it is not stored, the value is
    /// UNINITIALIZED_STACK_OFFSET.
    std::vector<int32_t> storedStackOffsets_;

    /// Index is the stack offset in bytes.
    std::deque<std::string> cachedStackOffsetOperandsPos_;
    /// Index is the absolute value of the stack offset in bytes.
    std::deque<std::string> cachedStackOffsetOperandsNeg_;

   public:
    explicit CodeGen(const IR::Module& ir, const TargetType targetType)
        : ir_(ir),
          targetType_(targetType),
          storedStackOffsets_(ir.getValuesCount(), UNINITIALIZED_STACK_OFFSET) {}

    [[nodiscard]] neutro::FastStringStream generate();

   private:
    [[nodiscard]] static uint32_t toBytes(const uint32_t sizeBits) { return (sizeBits + 7) / 8; }

    // --- Asm writing helpers
    void mov(int64_t srcVal, Reg dst);
    void mov(const std::string& src, Reg dst);
    void mov(Reg src, const std::string& dst);
    void mov(const int64_t srcVal, const std::string& dst, const uint32_t sizeBits) {
        const Reg reg{Reg::RAX, sizeBits};
        mov(srcVal, reg);
        mov(reg, dst);
    }
    void mov(const std::string& src, const std::string& dst, const uint32_t sizeBits) {
        const Reg reg{Reg::RAX, sizeBits};
        mov(src, reg);
        mov(reg, dst);
    }

    void lea(const std::string& loc, Reg::Name dst);

    void updateRsp();

    [[nodiscard]] static std::string deref(const std::string& loc) { return '[' + loc + ']'; }

    [[nodiscard]] static constexpr std::string_view ptrPrefix(uint32_t sizeBits) {
        sizeBits = (sizeBits + 7) / 8 * 8;

        switch (sizeBits) {
            case 8:
                return "byte ptr";
            case 16:
                return "word ptr";
            case 32:
                return "dword ptr";
            case 64:
                return "qword ptr";
            default:
                std::unreachable();
        }
    }

    [[nodiscard]] std::string stackOffsetOperand(int32_t stackOffsetBits);
    [[nodiscard]] std::string stackOffsetOperand(const uint32_t stackOffsetBits) {
        return stackOffsetOperand(static_cast<int32_t>(stackOffsetBits));
    }
    [[nodiscard]] static std::string getNameWithPrefix(const std::string_view name) {
        return "__" + std::string(name);
    }

    /// Creates a Reg with the size of the given value.
    [[nodiscard]] static Reg regForValue(const Reg::Name name, const IR::Value& value) {
        return Reg{name, value.getType().computeSizeBits()};
    }

    [[nodiscard]] static std::string labelForBasicBlockID(const uint32_t id) {
        return ".L" + std::to_string(id);
    }

   private:
    std::string stackAllocate(uint32_t sizeBits);
    std::string stackAllocate(const IR::Value& value);

    void loadTo(Reg reg, int32_t stackOffset);

    int32_t getStoredStackOffsetOrGenerate(const IR::Value* value);

    void generateValue(const IR::Value& value);

    void generateFunction(const IR::Function& func);
    void generateBasicBlock(const IR::BasicBlock& bb);
    void generateInstruction(const IR::Instruction& instr);
    void generateConstant(const IR::ConstantValue& constant);

    /// Stores the result in rax.
    void generateBinaryOperation(IR::OpCode opcode, Reg locA, const std::string& locB);
    /// Stores the result in rax.
    void generateBinaryOperation(IR::OpCode opcode, const std::string& locA,
                                 const std::string& locB, uint32_t sizeBits);
    void generateBinaryOperation(const IR::Instruction& binOp);

    void generateAlloca(const IR::Instruction& alloca);
    void generateLoad(const IR::Instruction& load);
    void generateStore(const IR::Instruction& store);
    void generateGep(const IR::Instruction& gep);

    void generateMemcpy(const IR::Instruction& memcpy);

    void generateBr(const IR::Instruction& br);

    void generateRet(const IR::Instruction& ret);

    void generateCall(const IR::Instruction& call);
    void generateSyscall(const IR::Instruction& sysc);
    void generateExit();
};

}  // namespace Backend
