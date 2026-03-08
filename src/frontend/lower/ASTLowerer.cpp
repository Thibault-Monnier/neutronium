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

IR::Type ASTLowerer::convertPrimitiveType(const Type& type) const {
    assert(type.isPrimitive());

    switch (type.primitive()) {
        case Primitive::Kind::INT:
        case Primitive::Kind::INT8:
        case Primitive::Kind::INT16:
        case Primitive::Kind::INT32:
        case Primitive::Kind::INT64:
            return IR::Type::intType(type.sizeBits(typeManager_));

        case Primitive::Kind::BOOL:
            return IR::Type::boolean();

        case Primitive::Kind::VOID:
            return IR::Type::voidType();

        case Primitive::Kind::UNKNOWN:
            break;
    }

    std::unreachable();
}

const IR::Type& ASTLowerer::convertType(const TypeID typeID) {
    const Type& type = typeManager_.getType(typeID);

    switch (type.kind()) {
        case TypeKind::PRIMITIVE: {
            const IR::Type irType = convertPrimitiveType(type);
            return builder_.registerType(irType);
        }

        case TypeKind::ARRAY: {
            const IR::Type& elementType = convertType(type.arrayElementTypeId());
            const IR::Type irArrayType = IR::Type::pointer(&elementType);
            return builder_.registerType(irArrayType);
        }

        case TypeKind::UNKNOWN:
            break;
    }

    std::unreachable();
}

void ASTLowerer::declareFunction(const std::string_view name,
                                 const std::span<AST::VariableDefinition*> parameters,
                                 const TypeID returnTypeID) {
    std::vector<const IR::Type*> parameterTypes;
    parameterTypes.reserve(parameters.size());
    for (const auto* param : parameters) {
        parameterTypes.push_back(&convertType(param->typeID_));
    }

    builder_.beginFunction(name, std::move(parameterTypes), convertType(returnTypeID));
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
            for (const auto* innerStmt : stmt.as<AST::BlockStatement>()->body_) {
                lowerStatement(*innerStmt);
            }
            break;

        default:
            std::unreachable();
    }
}

void ASTLowerer::lowerVariableDefinition(const AST::VariableDefinition& varDef) {
    const IR::Type& varType = convertType(varDef.typeID_);
    builder_.allocate(varDef.identifier_->name_, varType);

    IR::Value& value = lowerExpression(*varDef.value_);
    builder_.createStoreInstr(varDef.identifier_->name_, value);
}

void ASTLowerer::lowerAssignment(const AST::Assignment& assignment) {
    IR::Value& place = lowerExpression(*assignment.place_);
    IR::Value& value = lowerExpression(*assignment.value_);

    if (assignment.operator_ != AST::Operator::ASSIGN) {
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

        value = lowerBinaryExpression(*assignment.place_, *assignment.value_, compoundOp);
    }

    builder_.createStoreInstr(place, value);
}

void ASTLowerer::lowerExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    lowerExpression(*exprStmt.expression_);
}

void ASTLowerer::lowerIfStatement(const AST::IfStatement& ifStmt) {
    IR::Value& condition = lowerExpression(*ifStmt.condition_);

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
    IR::Value& condition = lowerExpression(*whileStmt.condition_);
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
    if (returnStmt.returnValue_ != nullptr) {
        IR::Value& returnValue = lowerExpression(*returnStmt.returnValue_);
        builder_.createRetInstr(returnValue);
    } else {
        builder_.createRetInstr();
    }
}

void ASTLowerer::lowerExitStatement(const AST::ExitStatement& exitStmt) {
    IR::Value& exitCode = lowerExpression(*exitStmt.exitCode_);
    builder_.createCallInstr("exit", {&exitCode});
}

IR::Value& ASTLowerer::lowerExpression(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return lowerNumberLiteral(*expr.as<AST::NumberLiteral>());
        case AST::NodeKind::BOOLEAN_LITERAL:
            return lowerBooleanLiteral(*expr.as<AST::BooleanLiteral>());
        case AST::NodeKind::IDENTIFIER:
            return lowerIdentifier(*expr.as<AST::Identifier>());
        case AST::NodeKind::FUNCTION_CALL:
            return lowerFunctionCall(*expr.as<AST::FunctionCall>());
        case AST::NodeKind::ARRAY_ACCESS:
            return lowerArrayAccess(*expr.as<AST::ArrayAccess>());
        case AST::NodeKind::ARRAY_LITERAL:
            return lowerArrayLiteral(*expr.as<AST::ArrayLiteral>());
        case AST::NodeKind::REPEAT_ARRAY_LITERAL:
            return lowerRepeatArrayLiteral(*expr.as<AST::RepeatArrayLiteral>());
        case AST::NodeKind::UNARY_EXPRESSION:
            return lowerUnaryExpression(*expr.as<AST::UnaryExpression>());
        case AST::NodeKind::BINARY_EXPRESSION:
            return lowerBinaryExpression(*expr.as<AST::BinaryExpression>());

        default:
            std::unreachable();
    }
}

IR::Value& ASTLowerer::lowerNumberLiteral(const AST::NumberLiteral& numberLit) {
    const IR::Type& type = convertType(numberLit.typeID_);
    return builder_.registerValue(IR::IntegerConstant{type, numberLit.value_});
}

IR::Value& ASTLowerer::lowerBooleanLiteral(const AST::BooleanLiteral& boolLit) {
    const IR::Type& type = convertType(boolLit.typeID_);
    return builder_.registerValue(IR::IntegerConstant{type, boolLit.value() ? 1 : 0});
}

IR::Value& ASTLowerer::lowerIdentifier(const AST::Identifier& identifier) {
    return builder_.createLoadInstr(identifier.name_);
}

IR::Value& ASTLowerer::lowerFunctionCall(const AST::FunctionCall& funcCall) {
    std::vector<IR::Value*> arguments;
    arguments.reserve(funcCall.arguments_.size());
    for (const auto* arg : funcCall.arguments_) {
        arguments.push_back(&lowerExpression(*arg));
    }
    return builder_.createCallInstr(funcCall.callee_->name_, std::move(arguments));
}

IR::Value& ASTLowerer::lowerArrayAccess(const AST::ArrayAccess& arrayAccess) {
    IR::Value& base = lowerExpression(*arrayAccess.base_);
    IR::Value& index = lowerExpression(*arrayAccess.index_);

    IR::Value& place = builder_.createGetElementPtrInstr(base, index);
    return builder_.createLoadInstr(place);
}

IR::Value& ASTLowerer::lowerArrayLiteral(const AST::ArrayLiteral& arrayLit) {
    // TODO: Implement this
    std::unreachable();
}

IR::Value& ASTLowerer::lowerRepeatArrayLiteral(const AST::RepeatArrayLiteral& repeatArrayLit) {
    // TODO: Implement this
    std::unreachable();
}

IR::Value& ASTLowerer::lowerUnaryExpression(const AST::UnaryExpression& unaryExpr) {
    assert(AST::isUnaryOperator(unaryExpr.operator_));

    IR::Value& operand = lowerExpression(*unaryExpr.operand_);

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

    IR::Value& leftVal = lowerExpression(left);
    IR::Value& rightVal = lowerExpression(right);

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

    IR::Value& leftVal = lowerExpression(left);
    builder_.createConditionalBranchInstr(leftVal, rightBlock, mergeBlock);

    builder_.setInsertionPoint(rightBlock);
    IR::Value& rightVal = lowerExpression(right);
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(mergeBlock);
    return builder_.createAndInstr(leftVal, rightVal);
}

IR::Value& ASTLowerer::lowerLogicalOrExpression(const AST::Expression& left,
                                                const AST::Expression& right) {
    IR::BasicBlock& rightBlock = builder_.createBasicBlock();
    IR::BasicBlock& mergeBlock = builder_.createBasicBlock();

    IR::Value& leftVal = lowerExpression(left);
    builder_.createConditionalBranchInstr(leftVal, mergeBlock, rightBlock);

    builder_.setInsertionPoint(rightBlock);
    IR::Value& rightVal = lowerExpression(right);
    builder_.createUnconditionalBranchInstr(mergeBlock);

    builder_.setInsertionPoint(mergeBlock);
    return builder_.createOrInstr(leftVal, rightVal);
}
