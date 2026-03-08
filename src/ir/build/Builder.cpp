#include "Builder.hpp"

#include <algorithm>
#include <cassert>
#include <ranges>

namespace IR {

void Builder::allocate(const std::string_view name, Type type) {
    [[maybe_unused]] auto [_, inserted] = allocated_.emplace(name, type);
    assert(inserted);
}

void Builder::beginFunction(std::string_view name, std::vector<Type>&& parameterTypes,
                            const Type returnType) {
    Function& func = module_.addFunction(Function(std::move(parameterTypes), returnType));
    functionTable_.emplace(name, func);
    currentFunction_ = &func;
}

Value& Builder::createStoreInstr(Value& location, Value& value) {
    assert(location.getType().getKind() == Type::Kind::PTR);
    std::vector<Value*> operands = {&location, &value};
    return addInstr({OpCode::STORE, Type::voidType(), std::move(operands)});
}

Value& Builder::createLoadInstr(Value& location) {
    assert(location.getType().getKind() == Type::Kind::PTR);
    std::vector<Value*> operands = {&location};
    return addInstr({OpCode::LOAD, *location.getType().getPointeeType(), std::move(operands)});
}

Value& Builder::createCallInstr(const std::string_view calleeName,
                                std::vector<Value*>&& arguments) {
    Function& callee = functionTable_.at(calleeName);

    // Assert the types from parameters and arguments are the same
    assert(std::ranges::equal(arguments | std::views::transform(&Value::getType),
                              callee.getParameterTypes()));

    std::vector<Value*> operands{&callee};
    operands.append_range(arguments);

    return addInstr({OpCode::CALL, callee.getType(), std::move(operands)});
}

Value& Builder::createRetInstr(Value& value) {
    // The return type should match the function's return type
    assert(currentFunction_->getType() == value.getType());

    std::vector<Value*> operands{&value};

    return addInstr({OpCode::RET, Type::voidType(), std::move(operands)});
}

Value& Builder::createConditionalBranchInstr(Value& condition, BasicBlock& trueBlock,
                                             BasicBlock& falseBlock) {
    std::vector<Value*> operands = {&condition, &trueBlock, &falseBlock};
    return addInstr({OpCode::BR, Type::voidType(), std::move(operands)});
}

Value& Builder::createUnconditionalBranchInstr(BasicBlock& targetBlock) {
    std::vector<Value*> operands = {&targetBlock};
    return addInstr({OpCode::BR, Type::voidType(), std::move(operands)});
}

Value& Builder::createArithmeticExpr(Value& a, Value& b, const OpCode opCode) {
    std::vector<Value*> operands = {&a, &b};

    const Type type = a.getType();
    assert(b.getType() == type);

    return addInstr({opCode, type, std::move(operands)});
}

}  // namespace IR
