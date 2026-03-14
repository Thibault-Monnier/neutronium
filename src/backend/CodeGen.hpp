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

    std::unordered_map<const IR::Value*, uint32_t> storedStackOffsets_;
    std::unordered_map<const IR::BasicBlock*, std::string> labels_;

   public:
    explicit CodeGen(const IR::Module& ir, const TargetType targetType)
        : ir_(ir), targetType_(targetType) {}

    [[nodiscard]] neutro::FastStringStream generate();

   private:
    // --- Asm writing helpers
    void mov(const std::string& src, const std::string& dst);
    void lea(const std::string& loc, const std::string& dst);

    static std::string deref(const std::string& loc) { return '[' + loc + ']'; }
    static std::string rax() { return "rax"; }
    static std::string rbx() { return "rbx"; }
    static std::string rdi() { return "rdi"; }

    static std::string stackOffsetOperand(uint32_t stackOffsetBits);
    static std::string getNameWithPrefix(std::string_view name);

   private:
    std::string stackAllocate(uint32_t sizeBits);
    std::string stackAllocate(const IR::Value& value);

    void loadToRax(uint32_t stackOffset);
    void loadToRbx(uint32_t stackOffset);
    void loadToRdi(uint32_t stackOffset);

    uint32_t getStoredStackOffsetOrGenerate(const IR::Value* value);

    void generateValue(const IR::Value& value);

    void generateFunction(const IR::Function& func);
    void generateBasicBlock(const IR::BasicBlock& bb);
    void generateInstruction(const IR::Instruction& instr);
    void generateConstant(const IR::ConstantValue& constant);

    void generateBinaryOperation(const IR::Instruction& binOp);

    void generateAlloca(const IR::Instruction& alloca);
    void generateLoad(const IR::Instruction& load);
    void generateStore(const IR::Instruction& store);
    void generateGep(const IR::Instruction& gep);

    void generateBr(const IR::Instruction& br);

    void generateRet(const IR::Instruction& ret);

    void generateSyscall(const IR::Instruction& sysc);
    void generateExit();
};

}  // namespace Backend
