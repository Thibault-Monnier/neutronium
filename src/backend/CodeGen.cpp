#include "CodeGen.hpp"

#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

#include "backend/Reg.hpp"
#include "driver/Cli.hpp"
#include "ir/core/IR.hpp"
#include "lib/FastStringStream.hpp"

namespace Backend {

neutro::FastStringStream CodeGen::generate() {
    output_ << ".intel_syntax noprefix\n\n";
    output_ << ".text\n\n";

    if (targetType_ == TargetType::EXECUTABLE) {
        output_ << ".globl _start\n";
        output_ << ".type _start, @function\n";
        output_ << "_start:\n";
        output_ << "push rbp\n";
        output_ << "mov rbp, rsp\n";
        output_ << "call " << getNameWithPrefix("main") << "\n";
        generateExit();
    }

    for (const IR::Function* func : ir_.getFunctions()) {
        if (func->isExternal()) continue;
        generateFunction(*func);
    }

    return std::move(output_);
}

void CodeGen::mov(const int64_t srcVal, const Reg dst) {
    const Reg dstReg{dst.getName(), 64};
    output_ << "mov " << dstReg.toString() << ", " << srcVal << "\n";
}

void CodeGen::mov(const StackOffset src, const Reg dst) {
    const uint32_t regSize = dst.sizeBits() == 32 ? 32 : 64;
    const Reg dstReg{dst.getName(), regSize};

    const bool isImplicitZx = dst.sizeBits() >= 32;
    const std::string_view instr = isImplicitZx ? "mov" : "movzx";

    output_ << instr << " " << dstReg.toString() << ", ";
    if (!isImplicitZx) {
        const std::string_view srcPrefix = ptrPrefix(dst.sizeBits());
        output_ << srcPrefix << " ";
    }
    output_ << getOperand(src) << "\n";
}

void CodeGen::mov(const Reg src, const StackOffset dst) {
    output_ << "mov " << getOperand(dst) << ", " << src.toString() << "\n";
}

void CodeGen::movToAddress(const Reg src, const Reg address) {
    output_ << "mov " << address.deref() << ", " << src.toString() << "\n";
}

void CodeGen::deref(const Reg reg, const Reg dst) {
    output_ << "mov " << dst.toString() << ", " << reg.deref() << "\n";
}

void CodeGen::lea(const StackOffset loc, const Reg::Name dst) {
    const Reg reg{dst, PTR_SIZE_BITS};
    output_ << "lea " << reg.toString() << ", " << getOperand(loc) << "\n";
}

std::string_view CodeGen::getOperand(const StackOffset stackOffset) {
    const int32_t bits = stackOffset.offsetBits();

    const bool isPos = bits >= 0;
    std::deque<std::string>& cache =
        isPos ? cachedStackOffsetOperandsPos_ : cachedStackOffsetOperandsNeg_;

    const size_t bytes = toBytes(std::abs(bits));
    const bool isOutOfBounds = bytes >= cache.size();
    if (isOutOfBounds || cache[bytes].empty()) [[unlikely]] {
        if (isOutOfBounds) cache.resize(bytes + 1);

        const std::string str = std::to_string(bytes);
        cache[bytes] = (isPos ? "[rbp - " : "[rbp + ") + str + "]";
    }

    return cache[bytes];
}

CodeGen::StackOffset CodeGen::stackAllocate(const uint32_t sizeBits) {
    stackOffset_ += toBytes(sizeBits) * 8;  // Align to bytes
    return stackOffset();
}

CodeGen::StackOffset CodeGen::stackAllocate(const IR::Value& value) {
    const StackOffset operand = stackAllocate(value.getType().computeSizeBits());
    storedStackOffsets_[value.getID()] = static_cast<int32_t>(stackOffset_);
    return operand;
}

CodeGen::StackOffset CodeGen::getStoredStackOffsetOrGenerate(const IR::Value* value) {
    const int32_t offset = storedStackOffsets_[value->getID()];
    if (offset != UNINITIALIZED_STACK_OFFSET) return StackOffset{offset};

    generateValue(*value);
    return StackOffset{storedStackOffsets_[value->getID()]};
}

void CodeGen::generateValue(const IR::Value& value) {
    if (auto* func = value.dynCast<const IR::Function>()) {
        generateFunction(*func);
    } else if (auto* bb = value.dynCast<const IR::BasicBlock>()) {
        generateBasicBlock(*bb);
    } else if (auto* instr = value.dynCast<const IR::Instruction>()) {
        generateInstruction(*instr);
    } else if (auto* constant = value.dynCast<const IR::ConstantValue>()) {
        generateConstant(*constant);
    } else if (auto* _ = value.dynCast<const IR::Argument>()) {
        // Should never have to generate an argument
        std::unreachable();
    } else {
        std::unreachable();
    }
}

void CodeGen::generateFunction(const IR::Function& func) {
    stackOffset_ = 0;

    output_ << "\n";

    const std::string& funcName = getNameWithPrefix(func.getName());
    if (func.isExported()) {
        output_ << ".globl " << funcName << "\n";
        output_ << ".type " << funcName << ", @function\n";
    }

    output_ << funcName << ":\n";
    output_ << "push rbp\n";
    output_ << "mov rbp, rsp\n";

    int32_t currOffset = -16 * 8;  // [rbp] is the saved rbp, [rbp + 8] is the return address
    for (const auto* arg : std::views::reverse(func.getArguments())) {
        storedStackOffsets_[arg->getID()] = currOffset;
        currOffset -=
            static_cast<int32_t>(arg->getType().computeSizeBytes()) * 8;  // Align to bytes
    }

    for (const IR::BasicBlock* bb = func.getFirstBasicBlock(); bb; bb = bb->getNext())
        generateBasicBlock(*bb);
}

void CodeGen::generateBasicBlock(const IR::BasicBlock& bb) {
    output_ << labelForBasicBlockID(bb.getID()) << ":\n";

    for (const auto* instr = bb.getFirstInstruction(); instr; instr = instr->getNext())
        generateInstruction(*instr);
}

void CodeGen::generateInstruction(const IR::Instruction& instr) {
    if (storedStackOffsets_[instr.getID()] != UNINITIALIZED_STACK_OFFSET) return;

    switch (instr.getOpcode()) {
        case IR::OpCode::ADD:
        case IR::OpCode::SUB:
        case IR::OpCode::MUL:
        case IR::OpCode::DIV:
        case IR::OpCode::AND:
        case IR::OpCode::OR:
        case IR::OpCode::XOR:
        case IR::OpCode::EQ:
        case IR::OpCode::LT:
        case IR::OpCode::LTE:
            generateBinaryOperation(instr);
            break;

        case IR::OpCode::ALLOCA:
            generateAlloca(instr);
            break;

        case IR::OpCode::LOAD:
            generateLoad(instr);
            break;

        case IR::OpCode::STORE:
            generateStore(instr);
            break;

        case IR::OpCode::GEP:
            generateGep(instr);
            break;

        case IR::OpCode::MEMCPY:
            generateMemcpy(instr);
            break;

        case IR::OpCode::BR:
            generateBr(instr);
            break;

        case IR::OpCode::CALL:
            generateCall(instr);
            break;

        case IR::OpCode::RET:
            generateRet(instr);
            break;

        case IR::OpCode::SYSCALL:
            generateSyscall(instr);
            break;
    }
}

void CodeGen::generateConstant(const IR::ConstantValue& constant) {
    if (storedStackOffsets_[constant.getID()] != UNINITIALIZED_STACK_OFFSET) return;

    if (auto* integerConst = constant.dynCast<const IR::IntegerConstant>()) {
        const StackOffset loc = stackAllocate(constant);
        const int64_t val = integerConst->getValue();
        mov(val, loc, constant.getType().computeSizeBits());
    } else {
        std::unreachable();
    }
}

namespace {
std::string_view computeBinaryOperationAsmCode(const IR::OpCode opcode) {
    assert(IR::isBinaryOp(opcode));
    switch (opcode) {
        case IR::OpCode::ADD:
            return "add";
        case IR::OpCode::SUB:
            return "sub";
        case IR::OpCode::MUL:
            return "imul";
        case IR::OpCode::DIV:
            return "idiv";
        case IR::OpCode::AND:
            return "and";
        case IR::OpCode::OR:
            return "or";
        case IR::OpCode::XOR:
            return "xor";
        case IR::OpCode::EQ:
        case IR::OpCode::LT:
        case IR::OpCode::LTE:
            return "cmp";
        default:
            std::unreachable();
    }
}

std::string_view computeBinaryComparisonAsmSuffix(const IR::OpCode opcode) {
    assert(IR::isBinaryComparisonOp(opcode));
    switch (opcode) {
        case IR::OpCode::EQ:
            return "e";
        case IR::OpCode::LT:
            return "l";
        case IR::OpCode::LTE:
            return "le";
        default:
            std::unreachable();
    }
}
}  // namespace

template <typename LocBType>
    requires std::same_as<LocBType, CodeGen::StackOffset> || std::same_as<LocBType, int64_t>
void CodeGen::generateBinaryOperation(const IR::OpCode opcode, Reg locA, const LocBType locB) {
    assert(IR::isBinaryOp(opcode));

    const std::string_view prefix = computeBinaryOperationAsmCode(opcode);

    if (opcode == IR::OpCode::DIV) {
        output_ << "cqo\n";
        if constexpr (std::same_as<LocBType, StackOffset>) {
            const std::string_view locBPrefix = ptrPrefix(locA.sizeBits());
            output_ << prefix << " " << locBPrefix << " " << getOperand(locB) << "\n";
        } else {
            // div doesn't support immediate values
            std::unreachable();
        }
    } else {
        if (opcode == IR::OpCode::MUL && locA.sizeBits() == 8) {
            // imul doesn't support 8-bit registers
            locA = Reg{locA.getName(), 64};
            constexpr Reg REG{Reg::RBX, 64};
            assert(locA.getName() != REG.getName());
            mov(locB, REG);
            output_ << prefix << " " << locA.toString() << ", " << REG.toString() << "\n";
        } else {
            if constexpr (std::same_as<LocBType, StackOffset>) {
                const std::string_view locBPrefix = ptrPrefix(locA.sizeBits());
                output_ << prefix << " " << locA.toString() << ", " << locBPrefix << " "
                        << getOperand(locB) << "\n";
            } else {
                output_ << prefix << " " << locA.toString() << ", " << locB << "\n";
            }
        }

        if (IR::isBinaryComparisonOp(opcode)) {
            const std::string_view suffix = computeBinaryComparisonAsmSuffix(opcode);
            output_ << "set" << suffix << " al\n";
            output_ << "movzx rax, al\n";
        }
    }
}

void CodeGen::generateBinaryOperation(const IR::OpCode opcode, const StackOffset locA,
                                      const StackOffset locB, const uint32_t sizeBits) {
    const Reg reg{Reg::RAX, sizeBits};
    mov(locA, reg);
    generateBinaryOperation(opcode, reg, locB);
}

void CodeGen::generateBinaryOperation(const IR::OpCode opcode, const StackOffset loc,
                                      const int64_t val, const uint32_t sizeBits) {
    const Reg reg{Reg::RAX, sizeBits};
    mov(loc, reg);
    generateBinaryOperation(opcode, reg, val);
}

void CodeGen::generateBinaryOperation(const IR::Instruction& binOp) {
    assert(binOp.getOperands().size() == 2);

    const IR::Value* operandA = binOp.getOperands()[0];
    const IR::Value* operandB = binOp.getOperands()[1];

    const StackOffset stackOffsetA = getStoredStackOffsetOrGenerate(operandA);
    const StackOffset stackOffsetB = getStoredStackOffsetOrGenerate(operandB);

    generateBinaryOperation(binOp.getOpcode(), stackOffsetA, stackOffsetB,
                            operandA->getType().computeSizeBits());

    const StackOffset loc = stackAllocate(binOp);
    mov(regForValue(Reg::RAX, binOp), loc);
}

void CodeGen::generateAlloca(const IR::Instruction& alloca) {
    assert(alloca.getOpcode() == IR::OpCode::ALLOCA);
    assert(alloca.getType().isPointer());

    const IR::Type& type = alloca.getType();
    const uint32_t elementSize = type.getSubtype().computeSizeBytes() * 8;

    assert(alloca.getOperands().size() == 1);
    const auto* nbElementsValue = alloca.getOperands()[0]->dynCast<const IR::IntegerConstant>();
    assert(nbElementsValue && "Dynamic alloca sizes are not supported yet");
    const uint32_t nbElements = nbElementsValue->getValue();

    const uint32_t allocateSize = elementSize * nbElements;
    const StackOffset allocatedLoc = stackAllocate(allocateSize);

    const StackOffset writeLoc = stackAllocate(alloca);
    const Reg reg = regForValue(Reg::RAX, alloca);
    lea(allocatedLoc, reg.getName());
    mov(reg, writeLoc);
}

void CodeGen::generateLoad(const IR::Instruction& load) {
    assert(load.getOpcode() == IR::OpCode::LOAD);
    assert(load.getOperands().size() == 1);

    const IR::Value* address = load.getOperands()[0];
    const StackOffset stackOffset = getStoredStackOffsetOrGenerate(address);
    const StackOffset writeLoc = stackAllocate(load);

    constexpr Reg ADDR_REG{Reg::RAX, PTR_SIZE_BITS};
    const Reg valReg = regForValue(Reg::RAX, load);

    mov(stackOffset, ADDR_REG);
    deref(ADDR_REG, valReg);
    mov(valReg, writeLoc);
}

void CodeGen::generateStore(const IR::Instruction& store) {
    assert(store.getOpcode() == IR::OpCode::STORE);
    assert(store.getOperands().size() == 2);

    const IR::Value* address = store.getOperands()[0];
    const IR::Value* value = store.getOperands()[1];

    const StackOffset addressStackOffset = getStoredStackOffsetOrGenerate(address);
    const StackOffset valueStackOffset = getStoredStackOffsetOrGenerate(value);

    constexpr Reg ADDR_REG{Reg::RAX, PTR_SIZE_BITS};
    const Reg valReg = regForValue(Reg::RBX, *value);

    mov(addressStackOffset, ADDR_REG);
    mov(valueStackOffset, valReg);
    movToAddress(valReg, ADDR_REG);
}

void CodeGen::generateGep(const IR::Instruction& gep) {
    assert(gep.getOpcode() == IR::OpCode::GEP);
    assert(gep.getOperands().size() == 2);

    const uint32_t elemSize = gep.getType().getSubtype().computeSizeBits();

    const IR::Value* base = gep.getOperands()[0];
    const IR::Value* idx = gep.getOperands()[1];

    const StackOffset baseStackOffset = getStoredStackOffsetOrGenerate(base);
    const StackOffset idxStackOffset = getStoredStackOffsetOrGenerate(idx);

    const Reg regA = regForValue(Reg::RAX, gep);

    generateBinaryOperation(IR::OpCode::MUL, idxStackOffset,
                            static_cast<int32_t>(toBytes(elemSize)),
                            idx->getType().computeSizeBits());
    generateBinaryOperation(IR::OpCode::ADD, regA, baseStackOffset);

    const StackOffset writeLoc = stackAllocate(gep);
    mov(regA, writeLoc);
}

void CodeGen::generateMemcpy(const IR::Instruction& memcpy) {
    assert(memcpy.getOpcode() == IR::OpCode::MEMCPY);
    assert(memcpy.getOperands().size() == 3);

    const IR::Value* dest = memcpy.getOperands()[0];
    const IR::Value* src = memcpy.getOperands()[1];
    const IR::Value* size = memcpy.getOperands()[2];

    const StackOffset destStackOffset = getStoredStackOffsetOrGenerate(dest);
    const StackOffset srcStackOffset = getStoredStackOffsetOrGenerate(src);
    const StackOffset sizeStackOffset = getStoredStackOffsetOrGenerate(size);

    mov(destStackOffset, regForValue(Reg::RDI, *dest));
    mov(srcStackOffset, regForValue(Reg::RSI, *src));
    mov(sizeStackOffset, regForValue(Reg::RCX, *size));

    output_ << "cld\n";
    output_ << "rep movsb\n";
}

void CodeGen::generateBr(const IR::Instruction& br) {
    assert(br.getOpcode() == IR::OpCode::BR);

    const size_t nbOps = br.getOperands().size();
    if (nbOps == 1) {  // Unconditional jump

        const auto* bb = br.getOperands()[0]->dynCast<const IR::BasicBlock>();
        assert(bb);
        output_ << "jmp " << labelForBasicBlockID(bb->getID()) << "\n";

    } else if (nbOps == 3) {  // Conditional jump

        const IR::Value* condition = br.getOperands()[0];
        const auto* bbTrue = br.getOperands()[1]->dynCast<const IR::BasicBlock>();
        const auto* bbFalse = br.getOperands()[2]->dynCast<const IR::BasicBlock>();
        assert(condition->getType().isBoolean());
        assert(bbTrue && bbFalse);

        const StackOffset conditionStackOffset = getStoredStackOffsetOrGenerate(condition);
        const Reg reg = regForValue(Reg::RAX, *condition);
        mov(conditionStackOffset, reg);
        output_ << "test " << reg.toString() << ", " << reg.toString() << "\n";
        output_ << "jne " << labelForBasicBlockID(bbTrue->getID()) << "\n";
        output_ << "jmp " << labelForBasicBlockID(bbFalse->getID()) << "\n";

    } else {
        std::unreachable();
    }
}

void CodeGen::generateRet(const IR::Instruction& ret) {
    assert(ret.getOpcode() == IR::OpCode::RET);

    if (ret.getOperands().empty()) {
        // Function is void, return whatever
    } else {
        assert(ret.getOperands().size() == 1);
        const IR::Value* val = ret.getOperands()[0];
        const StackOffset stackOffset = getStoredStackOffsetOrGenerate(val);
        mov(stackOffset, regForValue(Reg::RAX, *val));
    }

    output_ << "leave\n";
    output_ << "ret\n";
}

void CodeGen::generateCall(const IR::Instruction& call) {
    assert(call.getOpcode() == IR::OpCode::CALL);
    assert(call.getOperands().size() >= 1);

    const auto* callee = call.getOperands()[0]->dynCast<const IR::Function>();
    assert(callee);
    const std::string_view calleeName = callee->getName();

    callGenerationArgumentStackOffsets_.clear();
    for (const auto* arg : call.getOperands() | std::views::drop(1)) {
        const StackOffset stackOffset = getStoredStackOffsetOrGenerate(arg);
        callGenerationArgumentStackOffsets_.push_back(stackOffset);
    }

    for (size_t i = 0; i < callGenerationArgumentStackOffsets_.size(); ++i) {
        const IR::Value* arg = call.getOperands()[i + 1];
        assert(arg->getType().isPointer());

        const StackOffset stackOffset = callGenerationArgumentStackOffsets_[i];
        Reg reg = regForValue(Reg::RAX, *arg);
        mov(stackOffset, reg);

        const IR::Type* argType = &arg->getType();
        if (argType->getSubtype().isArray()) {
            // Arrays are passed as pointers, so we shouldn't dereference here
        } else {
            deref(reg, reg);
            argType = &arg->getType().getSubtype();
            reg = Reg{reg.getName(), argType->computeSizeBits()};
        }

        const StackOffset writeLoc = stackAllocate(argType->computeSizeBits());
        mov(reg, writeLoc);
    }

    updateRsp();

    output_ << "call " << getNameWithPrefix(calleeName) << "\n";

    if (call.getType().isVoid()) return;

    const StackOffset writeLoc = stackAllocate(call);
    mov(regForValue(Reg::RAX, call), writeLoc);
}

void CodeGen::generateSyscall(const IR::Instruction& sysc) {
    assert(sysc.getOpcode() == IR::OpCode::SYSCALL);
    assert(sysc.getOperands().size() == 2);

    const auto* syscNumberVal = sysc.getOperands()[0]->dynCast<const IR::IntegerConstant>();
    assert(syscNumberVal);
    const uint32_t syscNumber = syscNumberVal->getValue();
    assert(syscNumber == 60);  // Only `exit` is supported for now

    const IR::Value* val = sysc.getOperands()[1];
    const StackOffset stackOffset = getStoredStackOffsetOrGenerate(val);

    updateRsp();

    mov(stackOffset, regForValue(Reg::RDI, *val));
    mov(syscNumber, regForValue(Reg::RAX, *syscNumberVal));
    output_ << "syscall\n";
}

void CodeGen::generateExit() {
    output_ << "mov rdi, 0\n";   // exit code
    output_ << "mov rax, 60\n";  // syscall: exit
    output_ << "syscall\n";
}

}  // namespace Backend
