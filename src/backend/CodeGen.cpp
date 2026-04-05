#include "CodeGen.hpp"

#include <utility>

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

    for (const std::unique_ptr<IR::Function>& func : ir_.getFunctions()) {
        generateFunction(*func);
    }

    return std::move(output_);
}

void CodeGen::mov(const std::string& src, const std::string& dst) {
    output_ << "mov " << dst << ", " << src << "\n";
}

void CodeGen::lea(const std::string& loc, const std::string& dst) {
    output_ << "lea " << dst << ", " << loc << "\n";
}

std::string CodeGen::stackOffsetOperand(const uint32_t stackOffsetBits) {
    return "[rbp - " + std::to_string(stackOffsetBits / 8) + "]";
}

std::string CodeGen::getNameWithPrefix(const std::string_view name) {
    return "__" + std::string(name);
}

std::string CodeGen::stackAllocate(const uint32_t sizeBits) {
    // TODO: Handle non-multiples of 64 bits correctly
    stackOffset_ += (sizeBits + 63) / 64 * 64;
    return stackOffsetOperand(stackOffset_);
}

std::string CodeGen::stackAllocate(const IR::Value& value) {
    const std::string operand = stackAllocate(value.getType().computeSizeBits());
    storedStackOffsets_.emplace(&value, stackOffset_);
    return operand;
}

void CodeGen::loadToRax(const uint32_t stackOffset) {
    const std::string src = stackOffsetOperand(stackOffset);
    mov(src, rax());
}

void CodeGen::loadToRbx(const uint32_t stackOffset) {
    const std::string src = stackOffsetOperand(stackOffset);
    mov(src, rbx());
}

void CodeGen::loadToRdi(const uint32_t stackOffset) {
    const std::string src = stackOffsetOperand(stackOffset);
    mov(src, rdi());
}

uint32_t CodeGen::getStoredStackOffsetOrGenerate(const IR::Value* value) {
    const auto it = storedStackOffsets_.find(value);
    if (it != storedStackOffsets_.end()) return it->second;

    generateValue(*value);
    return storedStackOffsets_.at(value);
}

void CodeGen::generateValue(const IR::Value& value) {
    if (auto* func = dynamic_cast<const IR::Function*>(&value)) {
        generateFunction(*func);
    } else if (auto* bb = dynamic_cast<const IR::BasicBlock*>(&value)) {
        generateBasicBlock(*bb);
    } else if (auto* instr = dynamic_cast<const IR::Instruction*>(&value)) {
        generateInstruction(*instr);
    } else if (auto* constant = dynamic_cast<const IR::ConstantValue*>(&value)) {
        generateConstant(*constant);
    } else {
        std::unreachable();
    }
}

void CodeGen::generateFunction(const IR::Function& func) {
    stackOffset_ = 0;
    storedStackOffsets_.clear();

    output_ << "\n";

    const std::string& funcName = getNameWithPrefix(func.getName());
    if (func.isExported()) {
        output_ << ".globl " << funcName << "\n";
        output_ << ".type " << funcName << ", @function\n";
    }

    output_ << funcName << ":\n";
    output_ << "push rbp\n";
    output_ << "mov rbp, rsp\n";

    // Assign labels
    for (const auto& bb : func.getBasicBlocks()) {
        const size_t nbLabels = labels_.size();
        const std::string newLabel = ".L" + std::to_string(nbLabels);
        labels_.emplace(bb.get(), newLabel);
    }

    // Generate
    for (const auto& bb : func.getBasicBlocks()) generateBasicBlock(*bb);
}

void CodeGen::generateBasicBlock(const IR::BasicBlock& bb) {
    output_ << labels_.at(&bb) << ":\n";

    for (const auto* instr : bb.getInstructions()) generateInstruction(*instr);
}

void CodeGen::generateInstruction(const IR::Instruction& instr) {
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

        case IR::OpCode::BR:
            generateBr(instr);
            break;

        case IR::OpCode::CALL:
            // NYI
            std::unreachable();

        case IR::OpCode::RET:
            generateRet(instr);
            break;

        case IR::OpCode::SYSCALL:
            generateSyscall(instr);
            break;
    }
}

void CodeGen::generateConstant(const IR::ConstantValue& constant) {
    if (auto* integerConst = dynamic_cast<const IR::IntegerConstant*>(&constant)) {
        const std::string loc = stackAllocate(constant);
        const int64_t val = integerConst->getValue();
        mov(std::to_string(val), rax());
        mov(rax(), loc);
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

void CodeGen::generateBinaryOperation(const IR::OpCode opcode, const std::string& locA,
                                      const std::string& locB) {
    assert(IR::isBinaryOp(opcode));

    const std::string_view prefix = computeBinaryOperationAsmCode(opcode);
    if (opcode == IR::OpCode::DIV) {
        mov(locA, rax());
        output_ << "cqo\n";
        output_ << prefix << " qword ptr " << locB << "\n";
    } else {
        mov(locA, rax());
        output_ << prefix << " " << rax() << ", " << locB << "\n";

        if (IR::isBinaryComparisonOp(opcode)) {
            const std::string_view suffix = computeBinaryComparisonAsmSuffix(opcode);
            output_ << "set" << suffix << " al\n";
            output_ << "movzx rax, al\n";
        }
    }
}

void CodeGen::generateBinaryOperation(const IR::Instruction& binOp) {
    assert(binOp.getOperands().size() == 2);

    const IR::Value* operandA = binOp.getOperands()[0];
    const IR::Value* operandB = binOp.getOperands()[1];

    const int32_t stackOffsetA = getStoredStackOffsetOrGenerate(operandA);
    const int32_t stackOffsetB = getStoredStackOffsetOrGenerate(operandB);

    generateBinaryOperation(binOp.getOpcode(), stackOffsetOperand(stackOffsetA),
                            stackOffsetOperand(stackOffsetB));

    const std::string loc = stackAllocate(binOp);
    mov(rax(), loc);
}

void CodeGen::generateAlloca(const IR::Instruction& alloca) {
    assert(alloca.getOpcode() == IR::OpCode::ALLOCA);

    const IR::Type& type = alloca.getType();
    const uint32_t elementSize = type.getSubtype().computeSizeBits();

    assert(alloca.getOperands().size() == 1);
    const auto* nbElementsValue = dynamic_cast<const IR::IntegerConstant*>(alloca.getOperands()[0]);
    assert(nbElementsValue);
    const uint32_t nbElements = nbElementsValue->getValue();

    const uint32_t allocateSize = elementSize * nbElements;
    const std::string allocatedLoc = stackAllocate(allocateSize);

    const std::string writeLoc = stackAllocate(alloca);
    lea(allocatedLoc, rax());
    mov(rax(), writeLoc);
}

void CodeGen::generateLoad(const IR::Instruction& load) {
    assert(load.getOpcode() == IR::OpCode::LOAD);
    assert(load.getOperands().size() == 1);

    const IR::Value* address = load.getOperands()[0];
    const uint32_t stackOffset = getStoredStackOffsetOrGenerate(address);
    loadToRax(stackOffset);
    mov(deref(rax()), rax());

    const std::string writeLoc = stackAllocate(load);
    mov(rax(), writeLoc);
}

void CodeGen::generateStore(const IR::Instruction& store) {
    assert(store.getOpcode() == IR::OpCode::STORE);
    assert(store.getOperands().size() == 2);

    const IR::Value* address = store.getOperands()[0];
    const IR::Value* value = store.getOperands()[1];

    const uint32_t addressStackOffset = getStoredStackOffsetOrGenerate(address);
    const uint32_t valueStackOffset = getStoredStackOffsetOrGenerate(value);
    loadToRax(addressStackOffset);
    loadToRbx(valueStackOffset);
    mov(rbx(), deref(rax()));
}

void CodeGen::generateGep(const IR::Instruction& gep) {
    assert(gep.getOpcode() == IR::OpCode::GEP);
    assert(gep.getOperands().size() == 2);

    const uint32_t elemSize = gep.getType().getSubtype().computeSizeBits();

    const IR::Value* base = gep.getOperands()[0];
    const IR::Value* idx = gep.getOperands()[1];

    const uint32_t baseStackOffset = getStoredStackOffsetOrGenerate(base);
    const uint32_t idxStackOffset = getStoredStackOffsetOrGenerate(idx);

    generateBinaryOperation(IR::OpCode::MUL, stackOffsetOperand(idxStackOffset),
                            std::to_string(elemSize / 8));
    generateBinaryOperation(IR::OpCode::ADD, rax(), stackOffsetOperand(baseStackOffset));

    const std::string writeLoc = stackAllocate(gep);
    mov(rax(), writeLoc);
}

void CodeGen::generateBr(const IR::Instruction& br) {
    assert(br.getOpcode() == IR::OpCode::BR);

    const size_t nbOps = br.getOperands().size();
    if (nbOps == 1) {  // Unconditional jump

        const auto* bb = dynamic_cast<const IR::BasicBlock*>(br.getOperands()[0]);
        assert(bb);
        output_ << "jmp " << labels_.at(bb) << "\n";

    } else if (nbOps == 3) {  // Conditional jump

        const IR::Value* condition = br.getOperands()[0];
        const auto* bbTrue = dynamic_cast<const IR::BasicBlock*>(br.getOperands()[1]);
        const auto* bbFalse = dynamic_cast<const IR::BasicBlock*>(br.getOperands()[2]);
        assert(condition->getType().isBoolean());
        assert(bbTrue && bbFalse);

        const uint32_t conditionStackOffset = getStoredStackOffsetOrGenerate(condition);
        loadToRax(conditionStackOffset);
        output_ << "test " << rax() << ", " << rax() << "\n";
        output_ << "jne " << labels_.at(bbTrue) << "\n";
        output_ << "jmp " << labels_.at(bbFalse) << "\n";

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
        const uint32_t stackOffset = getStoredStackOffsetOrGenerate(val);
        loadToRax(stackOffset);
    }

    output_ << "leave\n";
    output_ << "ret\n";
}

void CodeGen::generateSyscall(const IR::Instruction& sysc) {
    assert(sysc.getOpcode() == IR::OpCode::SYSCALL);
    assert(sysc.getOperands().size() == 2);

    const auto* syscNumberVal = dynamic_cast<const IR::IntegerConstant*>(sysc.getOperands()[0]);
    assert(syscNumberVal);
    const uint32_t syscNumber = syscNumberVal->getValue();
    assert(syscNumber == 60);  // Only `exit` is supported for now

    const IR::Value* val = sysc.getOperands()[1];
    const uint32_t stackOffset = getStoredStackOffsetOrGenerate(val);

    loadToRdi(stackOffset);
    mov(std::to_string(syscNumber), rax());
    output_ << "syscall\n";
}

void CodeGen::generateExit() {
    output_ << "mov rdi, 0\n";   // exit code
    output_ << "mov rax, 60\n";  // syscall: exit
    output_ << "syscall\n";
}

}  // namespace Backend
