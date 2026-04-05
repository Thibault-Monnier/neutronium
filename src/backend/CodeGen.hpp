#pragma once

#include <unordered_map>

#include "driver/Cli.hpp"
#include "ir/core/IR.hpp"
#include "lib/FastStringStream.hpp"

namespace Backend {

/// Consumes IR and produces X86-64 assembly.
class CodeGen {
    const IR::Module& ir_;

    neutro::FastStringStream output_;

    const TargetType targetType_;

    /// In bits.
    uint32_t stackOffset_ = 0;

    std::unordered_map<const IR::Value*, int32_t> storedStackOffsets_;
    std::unordered_map<const IR::BasicBlock*, std::string> labels_;

   public:
    explicit CodeGen(const IR::Module& ir, const TargetType targetType)
        : ir_(ir), targetType_(targetType) {}

    [[nodiscard]] neutro::FastStringStream generate();

   private:
    // --- Asm writing helpers
    void mov(const std::string& src, const std::string& dst);
    void lea(const std::string& loc, const std::string& dst);

    void updateRsp();

    [[nodiscard]] static std::string deref(const std::string& loc) { return '[' + loc + ']'; }
    [[nodiscard]] static std::string rax() { return "rax"; }
    [[nodiscard]] static std::string rbx() { return "rbx"; }
    [[nodiscard]] static std::string rdi() { return "rdi"; }

    [[nodiscard]] static std::string stackOffsetOperand(int32_t stackOffsetBits);
    [[nodiscard]] static std::string stackOffsetOperand(const uint32_t stackOffsetBits) {
        return stackOffsetOperand(static_cast<int32_t>(stackOffsetBits));
    }
    [[nodiscard]] static std::string getNameWithPrefix(std::string_view name);

   private:
    std::string stackAllocate(uint32_t sizeBits);
    std::string stackAllocate(const IR::Value& value);

    void loadToRax(int32_t stackOffset);
    void loadToRbx(int32_t stackOffset);
    void loadToRdi(int32_t stackOffset);

    int32_t getStoredStackOffsetOrGenerate(const IR::Value* value);

    void generateValue(const IR::Value& value);

    void generateFunction(const IR::Function& func);
    void generateBasicBlock(const IR::BasicBlock& bb);
    void generateInstruction(const IR::Instruction& instr);
    void generateConstant(const IR::ConstantValue& constant);

    /// Stores the result in rax.
    void generateBinaryOperation(IR::OpCode opcode, const std::string& locA,
                                 const std::string& locB);
    void generateBinaryOperation(const IR::Instruction& binOp);

    void generateAlloca(const IR::Instruction& alloca);
    void generateLoad(const IR::Instruction& load);
    void generateStore(const IR::Instruction& store);
    void generateGep(const IR::Instruction& gep);

    void generateBr(const IR::Instruction& br);

    void generateRet(const IR::Instruction& ret);

    void generateCall(const IR::Instruction& call);
    void generateSyscall(const IR::Instruction& sysc);
    void generateExit();
};

}  // namespace Backend
