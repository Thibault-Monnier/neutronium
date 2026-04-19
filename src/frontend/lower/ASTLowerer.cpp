#include "ASTLowerer.hpp"

IR::Module&& ASTLowerer::lower() {
    for (const auto& externalFuncDecl : ast_.externalFunctions_) {
        lowerExternalFunction(*externalFuncDecl);
    }

    for (const auto* funcDef : ast_.functions_) {
        lowerFunction(*funcDef);
    }

    return std::move(ir_);
}

const IR::Type& ASTLowerer::convertPrimitiveType(const Type& type) const {
    assert(type.isPrimitive());

    switch (type.primitive()) {
        case Primitive::Kind::INT:
        case Primitive::Kind::INT8:
        case Primitive::Kind::INT16:
        case Primitive::Kind::INT32:
        case Primitive::Kind::INT64:
            return builder_.intType(type.sizeBits(typeManager_));

        case Primitive::Kind::BOOL:
            return builder_.boolType();

        case Primitive::Kind::VOID:
            return builder_.voidType();

        case Primitive::Kind::UNKNOWN:
            break;
    }

    std::unreachable();
}

const IR::Type& ASTLowerer::convertType(const TypeID typeID) {
    const Type& type = typeManager_.getType(typeID);

    switch (type.kind()) {
        case TypeKind::PRIMITIVE:
            return convertPrimitiveType(type);

        case TypeKind::ARRAY: {
            const IR::Type& elementType = convertType(type.arrayElementTypeId());
            const uint32_t elementCount = type.arrayLength();
            return builder_.arrayType(elementType, elementCount);
        }

        case TypeKind::UNKNOWN:
            break;
    }

    std::unreachable();
}

void ASTLowerer::declareSymbol(std::string_view name, IR::Value* value) {
    [[maybe_unused]] auto [_, inserted] = scopedSymbolAdresses_.back().emplace(name, value);
    assert(inserted);
}

IR::Value& ASTLowerer::lookupSymbolAddress(const std::string_view name) const {
    // Innermost scopes have a higher chance of containing the symbol
    for (const auto& scope : std::ranges::reverse_view(scopedSymbolAdresses_)) {
        const auto it = scope.find(name);
        if (it != scope.end()) return *it->second;
    }

    std::unreachable();
}

void ASTLowerer::declareFunction(const std::string_view name,
                                 const std::span<AST::VariableDefinition*> parameters,
                                 const TypeID returnTypeID, const bool isExported,
                                 const bool isExternal) {
    std::vector<IR::Argument*> args;
    bool usingHiddenReturnPointer = false;

    const IR::Type* returnType = &convertType(returnTypeID);
    if (returnType->isArray()) {
        // Pass the pointer as a hidden first argument, return type becomes void.
        returnType = &builder_.voidType();
        const IR::Type& argType = builder_.ptrType(convertType(returnTypeID));
        args.push_back(&builder_.createArgument(argType));
        usingHiddenReturnPointer = true;
    }

    for (const auto* param : parameters) {
        const IR::Type* paramType = &convertType(param->typeID_);

        if (paramType->isArray()) {
            // For arrays, we pass a pointer to the array.
            paramType = &builder_.ptrType(*paramType);
        }

        IR::Argument& arg = builder_.createArgument(*paramType);
        args.push_back(&arg);
    }

    const IR::Function& func =
        builder_.beginFunction(name, std::move(args), *returnType, isExported, isExternal);

    if (isExternal) return;

    for (size_t i = 0; i < parameters.size(); ++i) {
        const auto* param = parameters[i];
        IR::Argument* arg = func.getArguments()[i + (usingHiddenReturnPointer ? 1 : 0)];

        IR::Value& address = builder_.createAllocaInstr(arg->getType());
        builder_.createStoreInstr(address, *arg);

        declareSymbol(param->identifier_->name_, &address);
    }
}

void ASTLowerer::lowerFunction(const AST::FunctionDefinition& funcDef) {
    const ScopeGuard scopeGuard(*this);  // For the parameters

    const TypeID returnTypeID = funcDef.returnTypeID_;
    declareFunction(funcDef.identifier_->name_, funcDef.parameters_, returnTypeID,
                    funcDef.isExported(), false);

    lowerStatement(*funcDef.body_);

    const Type& type = typeManager_.getType(returnTypeID);
    if (type.isVoid()) {
        // Add a trailing return in case there isn't one before
        builder_.createRetInstr();
    }
}

void ASTLowerer::lowerStatement(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION:
            lowerVariableDefinition(*stmt.as<AST::VariableDefinition>());
            break;
        case AST::NodeKind::ASSIGNMENT:
            lowerAssignment(*stmt.as<AST::Assignment>());
            break;
        case AST::NodeKind::EXPRESSION_STATEMENT:
            lowerExpressionStatement(*stmt.as<AST::ExpressionStatement>());
            break;
        case AST::NodeKind::IF_STATEMENT:
            lowerIfStatement(*stmt.as<AST::IfStatement>());
            break;
        case AST::NodeKind::WHILE_STATEMENT:
            lowerWhileStatement(*stmt.as<AST::WhileStatement>());
            break;
        case AST::NodeKind::BREAK_STATEMENT:
            lowerBreakStatement();
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            lowerContinueStatement();
            break;
        case AST::NodeKind::RETURN_STATEMENT:
            lowerReturnStatement(*stmt.as<AST::ReturnStatement>());
            break;
        case AST::NodeKind::EXIT_STATEMENT:
            lowerExitStatement(*stmt.as<AST::ExitStatement>());
            break;
        case AST::NodeKind::BLOCK_STATEMENT:
            enterScope();
            for (const auto* innerStmt : stmt.as<AST::BlockStatement>()->body_) {
                lowerStatement(*innerStmt);
            }
            exitScope();
            break;

        default:
            std::unreachable();
    }
}

void ASTLowerer::lowerVariableDefinition(const AST::VariableDefinition& varDef) {
    const IR::Type& varType = convertType(varDef.typeID_);
    IR::Value& address = builder_.createAllocaInstr(varType);
    declareSymbol(varDef.identifier_->name_, &address);

    lowerValueExpression(*varDef.value_, &address);
}

void ASTLowerer::lowerAssignment(const AST::Assignment& assignment) {
    IR::Value& place = lowerPlaceExpression(*assignment.place_);

    if (assignment.operator_ == AST::Operator::ASSIGN) {
        lowerValueExpression(*assignment.value_, &place);
    } else {
        AST::Operator compoundOp;
        if (assignment.operator_ == AST::Operator::ADD_ASSIGN)
            compoundOp = AST::Operator::ADD;
        else if (assignment.operator_ == AST::Operator::SUBTRACT_ASSIGN)
            compoundOp = AST::Operator::SUBTRACT;
        else if (assignment.operator_ == AST::Operator::MULTIPLY_ASSIGN)
            compoundOp = AST::Operator::MULTIPLY;
        else if (assignment.operator_ == AST::Operator::DIVIDE_ASSIGN)
            compoundOp = AST::Operator::DIVIDE;
        else
            std::unreachable();

        IR::Value& value =
            lowerBinaryExpression(*assignment.place_, *assignment.value_, compoundOp);
        builder_.createStoreInstr(place, value);
    }
}

void ASTLowerer::lowerExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    lowerValueExpression(*exprStmt.expression_);
}

void ASTLowerer::lowerIfStatement(const AST::IfStatement& ifStmt) {
    IR::Value& condition = lowerValueExpression(*ifStmt.condition_);

    IR::BasicBlock& thenBlock = builder_.createBasicBlock();
    IR::BasicBlock& elseBlock = builder_.createBasicBlock();
    IR::BasicBlock& mergeBlock = builder_.createBasicBlock();

    builder_.createConditionalBranchInstr(condition, thenBlock, elseBlock);

    builder_.setInsertionPoint(thenBlock);
    lowerStatement(*ifStmt.body_);
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(elseBlock);
    if (ifStmt.elseClause_ != nullptr) {
        lowerStatement(*ifStmt.elseClause_);
    }
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(mergeBlock);
}

void ASTLowerer::lowerWhileStatement(const AST::WhileStatement& whileStmt) {
    IR::BasicBlock& conditionBlock = builder_.createBasicBlock();
    IR::BasicBlock& bodyBlock = builder_.createBasicBlock();
    IR::BasicBlock& mergeBlock = builder_.createBasicBlock();

    IR::BasicBlock* previousBreakBlock = currentBreakBlock_;
    IR::BasicBlock* previousContinueBlock = currentContinueBlock_;

    currentBreakBlock_ = &mergeBlock;
    currentContinueBlock_ = &conditionBlock;

    builder_.createUnconditionalBranchInstr(conditionBlock);

    builder_.setInsertionPoint(conditionBlock);
    IR::Value& condition = lowerValueExpression(*whileStmt.condition_);
    builder_.createConditionalBranchInstr(condition, bodyBlock, mergeBlock);

    builder_.setInsertionPoint(bodyBlock);
    lowerStatement(*whileStmt.body_);
    builder_.createUnconditionalBranchInstr(conditionBlock);

    builder_.setInsertionPoint(mergeBlock);
    currentBreakBlock_ = previousBreakBlock;
    currentContinueBlock_ = previousContinueBlock;
}

void ASTLowerer::lowerBreakStatement() {
    assert(currentBreakBlock_ != nullptr);
    builder_.createUnconditionalBranchInstr(*currentBreakBlock_);
}

void ASTLowerer::lowerContinueStatement() {
    assert(currentContinueBlock_ != nullptr);
    builder_.createUnconditionalBranchInstr(*currentContinueBlock_);
}

void ASTLowerer::lowerReturnStatement(const AST::ReturnStatement& returnStmt) {
    if (!returnStmt.returnValue_) {
        builder_.createRetInstr();
        return;
    }

    const Type& returnType = typeManager_.getType(returnStmt.returnValue_->typeID_);
    if (returnType.isArray()) {
        // Arrays are returned via a hidden pointer argument, so we should write there.
        IR::Value* returnValueAddress = builder_.getCurrentFunction().getArguments()[0];
        lowerValueExpression(*returnStmt.returnValue_, returnValueAddress);
        builder_.createRetInstr();
        return;
    }

    IR::Value& returnValue = lowerValueExpression(*returnStmt.returnValue_);
    builder_.createRetInstr(returnValue);
}

void ASTLowerer::lowerExitStatement(const AST::ExitStatement& exitStmt) {
    IR::Value& exitCode = lowerValueExpression(*exitStmt.exitCode_);
    builder_.createSyscallInstr(60, {&exitCode});
}

IR::Value& ASTLowerer::lowerValueExpression(const AST::Expression& expr,
                                            const std::optional<IR::Value*> place) {
    IR::Value* value;
    switch (expr.kind_) {
        // Always values
        case AST::NodeKind::NUMBER_LITERAL:
            value = &lowerNumberLiteral(*expr.as<AST::NumberLiteral>());
            break;
        case AST::NodeKind::BOOLEAN_LITERAL:
            value = &lowerBooleanLiteral(*expr.as<AST::BooleanLiteral>());
            break;
        case AST::NodeKind::FUNCTION_CALL:
            value = &lowerFunctionCall(*expr.as<AST::FunctionCall>());
            break;
        case AST::NodeKind::UNARY_EXPRESSION:
            value = &lowerUnaryExpression(*expr.as<AST::UnaryExpression>());
            break;
        case AST::NodeKind::BINARY_EXPRESSION:
            value = &lowerBinaryExpression(*expr.as<AST::BinaryExpression>());
            break;

        case AST::NodeKind::ARRAY_LITERAL:
            return lowerArrayLiteral(*expr.as<AST::ArrayLiteral>(), place);
        case AST::NodeKind::REPEAT_ARRAY_LITERAL:
            return lowerRepeatArrayLiteral(*expr.as<AST::RepeatArrayLiteral>(), place);

        // Places that need to be loaded
        case AST::NodeKind::IDENTIFIER:
        case AST::NodeKind::ARRAY_ACCESS: {
            IR::Value& address = lowerPlaceExpression(expr);

            if (address.getType().holdsSubtype() && address.getType().getSubtype().isArray()) {
                // Don't load arrays
                value = &address;
            } else {
                value = &builder_.createLoadInstr(address);
            }

            break;
        }

        default:
            std::unreachable();
    }

    if (place) {
        const IR::Type& type = value->getType();
        if (type.holdsSubtype() && type.getSubtype().isArray()) {
            // For arrays, we use memcpy.
            const uint32_t size = type.getSubtype().computeSizeBytes();
            IR::Value& arraySizeBytes = builder_.createIntegerConstant(builder_.intType(64), size);
            builder_.createMemcpyInstr(*place.value(), *value, arraySizeBytes);
        } else {
            builder_.createStoreInstr(*place.value(), *value);
        }
    }

    return *value;
}

IR::Value& ASTLowerer::lowerPlaceExpression(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::IDENTIFIER:
            return lowerIdentifierAddress(*expr.as<AST::Identifier>());
        case AST::NodeKind::ARRAY_ACCESS:
            return lowerArrayAccessAddress(*expr.as<AST::ArrayAccess>());

        default:
            std::unreachable();
    }
}

IR::Value& ASTLowerer::lowerNumberLiteral(const AST::NumberLiteral& numberLit) {
    const IR::Type& type = convertType(numberLit.typeID_);
    return builder_.createIntegerConstant(type, numberLit.value_);
}

IR::Value& ASTLowerer::lowerBooleanLiteral(const AST::BooleanLiteral& boolLit) {
    return builder_.createBooleanConstant(boolLit.value());
}

IR::Value& ASTLowerer::lowerIdentifierAddress(const AST::Identifier& identifier) const {
    return lookupSymbolAddress(identifier.name_);
}

IR::Value& ASTLowerer::lowerFunctionCall(const AST::FunctionCall& funcCall) {
    std::vector<IR::Value*> arguments;

    const Type& returnType = typeManager_.getType(funcCall.typeID_);
    if (returnType.isArray()) {
        // For arrays, allocate space and pass a pointer as a hidden first argument.
        IR::Value& returnValueAddress = builder_.createAllocaInstr(convertType(funcCall.typeID_));
        arguments.push_back(&returnValueAddress);
    }

    for (const auto* arg : funcCall.arguments_) {
        const IR::Type& type = convertType(arg->typeID_);
        IR::Value& address = builder_.createAllocaInstr(type);
        lowerValueExpression(*arg, &address);

        arguments.push_back(&address);
    }

    IR::Value& call = builder_.createCallInstr(funcCall.callee_->name_, std::move(arguments));

    if (returnType.isArray()) {
        return *arguments[0];
    } else {
        return call;
    }
}

IR::Value& ASTLowerer::lowerArrayAccessAddress(const AST::ArrayAccess& arrayAccess) {
    IR::Value& array = lowerValueExpression(*arrayAccess.base_);
    IR::Value& index = lowerValueExpression(*arrayAccess.index_);

    return builder_.createGetElementPtrInstr(array, index);
}

IR::Value& ASTLowerer::lowerArrayLiteral(const AST::ArrayLiteral& arrayLit,
                                         const std::optional<IR::Value*> place) {
    const IR::Type& type = convertType(arrayLit.typeID_);

    IR::Value& arrayPtr = place ? *place.value() : builder_.createAllocaInstr(type);

    for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
        const IR::Type& indexType = builder_.intType(64);
        IR::Value& indexValue = builder_.createIntegerConstant(indexType, static_cast<int64_t>(i));
        IR::Value& elementPtr = builder_.createGetElementPtrInstr(arrayPtr, indexValue);

        lowerValueExpression(*arrayLit.elements_[i], &elementPtr);
    }

    return arrayPtr;
}

IR::Value& ASTLowerer::lowerRepeatArrayLiteral(const AST::RepeatArrayLiteral& repeatArrayLit,
                                               const std::optional<IR::Value*> place) {
    const IR::Type& type = convertType(repeatArrayLit.typeID_);

    IR::Value& arrayPtr = place ? *place.value() : builder_.createAllocaInstr(type);
    IR::Value& elementValue = lowerValueExpression(*repeatArrayLit.element_);

    const int64_t count = repeatArrayLit.count_->value_;
    assert(count > 0);

    for (size_t i = 0; i < static_cast<size_t>(count); ++i) {
        const IR::Type& indexType = builder_.intType(64);
        IR::Value& indexValue = builder_.createIntegerConstant(indexType, static_cast<int64_t>(i));
        IR::Value& elementPtr = builder_.createGetElementPtrInstr(arrayPtr, indexValue);
        builder_.createStoreInstr(elementPtr, elementValue);
        // TODO: Handle non-scalar elements (e.g. for multidimensional arrays) by using memcpy.
    }

    return arrayPtr;
}

IR::Value& ASTLowerer::lowerUnaryExpression(const AST::UnaryExpression& unaryExpr) {
    assert(AST::isUnaryOperator(unaryExpr.operator_));

    IR::Value& operand = lowerValueExpression(*unaryExpr.operand_);

    switch (unaryExpr.operator_) {
        case AST::Operator::ADD:
            return operand;  // Nothing to do
        case AST::Operator::SUBTRACT:
            return builder_.createNegInstr(operand);
        case AST::Operator::LOGICAL_NOT:
            return builder_.createNotInstr(operand);

        default:
            std::unreachable();
    }
}

IR::Value& ASTLowerer::lowerBinaryExpression(const AST::Expression& left,
                                             const AST::Expression& right, const AST::Operator op) {
    // Handle short-circuiting operators separately
    if (op == AST::Operator::LOGICAL_AND) return lowerLogicalAndExpression(left, right);
    if (op == AST::Operator::LOGICAL_OR) return lowerLogicalOrExpression(left, right);

    IR::Value& leftVal = lowerValueExpression(left);
    IR::Value& rightVal = lowerValueExpression(right);

    switch (op) {
        case AST::Operator::ADD:
            return builder_.createAddInstr(leftVal, rightVal);
        case AST::Operator::SUBTRACT:
            return builder_.createSubInstr(leftVal, rightVal);
        case AST::Operator::MULTIPLY:
            return builder_.createMulInstr(leftVal, rightVal);
        case AST::Operator::DIVIDE:
            return builder_.createDivInstr(leftVal, rightVal);

        case AST::Operator::EQUALS:
            return builder_.createEqInstr(leftVal, rightVal);
        case AST::Operator::NOT_EQUALS:
            return builder_.createNotEqInstr(leftVal, rightVal);
        case AST::Operator::LESS_THAN:
            return builder_.createLtInstr(leftVal, rightVal);
        case AST::Operator::LESS_THAN_OR_EQUAL:
            return builder_.createLteInstr(leftVal, rightVal);
        case AST::Operator::GREATER_THAN:
            return builder_.createGtInstr(leftVal, rightVal);
        case AST::Operator::GREATER_THAN_OR_EQUAL:
            return builder_.createGteInstr(leftVal, rightVal);

        default:
            std::unreachable();
    }
}

IR::Value& ASTLowerer::lowerLogicalAndExpression(const AST::Expression& left,
                                                 const AST::Expression& right) {
    IR::BasicBlock& rightBlock = builder_.createBasicBlock();
    IR::BasicBlock& mergeBlock = builder_.createBasicBlock();

    IR::Value& leftVal = lowerValueExpression(left);
    builder_.createConditionalBranchInstr(leftVal, rightBlock, mergeBlock);

    builder_.setInsertionPoint(rightBlock);
    IR::Value& rightVal = lowerValueExpression(right);
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(mergeBlock);
    return builder_.createAndInstr(leftVal, rightVal);
}

IR::Value& ASTLowerer::lowerLogicalOrExpression(const AST::Expression& left,
                                                const AST::Expression& right) {
    IR::BasicBlock& rightBlock = builder_.createBasicBlock();
    IR::BasicBlock& mergeBlock = builder_.createBasicBlock();

    IR::Value& leftVal = lowerValueExpression(left);
    builder_.createConditionalBranchInstr(leftVal, mergeBlock, rightBlock);

    builder_.setInsertionPoint(rightBlock);
    IR::Value& rightVal = lowerValueExpression(right);
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(mergeBlock);
    return builder_.createOrInstr(leftVal, rightVal);
}
