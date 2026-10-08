#include "ConstraintGenerator.hpp"

#include <ranges>

using namespace Sema;

void ConstraintGenerator::equalityConstraint(TypeID a, TypeID b, const AST::Node& node) const {
    typeSolver_.addConstraint<EqualityConstraint>(a, b, node);
}

void ConstraintGenerator::traitConstraint(TypeID type, Trait trait, const AST::Node& node) const {
    typeSolver_.addConstraint<HasTraitConstraint>(type, trait, node);
}

void ConstraintGenerator::subscriptConstraint(TypeID arrayType, TypeID elementType,
                                              const AST::Node& node) const {
    typeSolver_.addConstraint<SubscriptConstraint>(arrayType, elementType, node);
}

void ConstraintGenerator::storableConstraint(TypeID type, const AST::Node& node) const {
    // TODO: remove this constraint and manually check in the Validator
    typeSolver_.addConstraint<StorableConstraint>(type, node);
}

void ConstraintGenerator::traverse() {
    for (const auto& externalFuncDecl : ast_->externalFunctions_) {
        analyseExternalFunctionDeclaration(*externalFuncDecl);
    }

    for (const auto& funcDef : ast_->functions_) {
        analyseFunctionDefinition(*funcDef);
    }
}

void ConstraintGenerator::emitErrorsAndQuit() const {
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}

void ConstraintGenerator::error(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex(),
                                   fileID_);
}

void ConstraintGenerator::fatalError(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex(),
                                   fileID_);
    emitErrorsAndQuit();
}

void ConstraintGenerator::enterScope() { scopeBoundaries_.push_back(scopedVariables_.size()); }

void ConstraintGenerator::exitScope() {
    scopedVariables_.erase(scopedVariables_.begin() + scopeBoundaries_.back(),
                           scopedVariables_.end());
    scopeBoundaries_.pop_back();
}

__attribute__((cold)) void ConstraintGenerator::handleUndeclaredSymbolError(
    std::string_view name, const AST::Node& node, [[maybe_unused]] const SymbolKind kind) const {
    const auto info = getSymbolInfo(name);
    if (info) {
        assert(info->kind() != kind);
        switch (info->kind()) {
            case SymbolKind::FUNCTION:
                error(std::format("Function `{}` used as variable", name), node);
                break;
            case SymbolKind::VARIABLE:
                error(std::format("Attempted to call a non-function: `{}`", name), node);
                break;
        }

    } else {
        error(std::format("Undeclared symbol: `{}`", name), node);
    }
}

std::optional<SymbolInfo> ConstraintGenerator::getFunctionSymbolInfo(
    const std::string_view name) const {
    const auto it = functionsTable_.find(name);
    if (it != functionsTable_.end()) return it->second;
    return std::nullopt;
}

std::optional<SymbolInfo> ConstraintGenerator::getVariableSymbolInfo(
    const std::string_view name) const {
    // Last declared symbols have a higher chance of being what we're looking for
    for (const auto& [symName, value] : std::ranges::reverse_view(scopedVariables_)) {
        if (symName == name) return value;
    }

    return std::nullopt;
}

std::optional<SymbolInfo> ConstraintGenerator::getSymbolInfo(const std::string_view name) const {
    const auto varInfo = getVariableSymbolInfo(name);
    if (varInfo) return varInfo;

    const auto funcInfo = getFunctionSymbolInfo(name);
    if (funcInfo) return funcInfo;

    return std::nullopt;
}

std::optional<SymbolInfo> ConstraintGenerator::getFunctionSymbolInfoOrError(
    const std::string_view name, const AST::Node& node) const {
    const auto info = getFunctionSymbolInfo(name);

    if (!info) handleUndeclaredSymbolError(name, node, SymbolKind::FUNCTION);
    return info;
}

std::optional<SymbolInfo> ConstraintGenerator::getVariableSymbolInfoOrError(
    const std::string_view name, const AST::Node& node) const {
    const auto info = getVariableSymbolInfo(name);

    if (!info) handleUndeclaredSymbolError(name, node, SymbolKind::VARIABLE);
    return info;
}

void ConstraintGenerator::ensureSymbolUndeclaredOrError(const AST::Node* declarationNode,
                                                        const std::string_view name) const {
    if (getSymbolInfo(name)) {
        fatalError(std::format("Redeclaration of symbol: `{}`", name), *declarationNode);
    }
}

void ConstraintGenerator::handleFunctionDeclaration(
    const AST::Node* declNode, const std::string_view name,
    const std::span<AST::VariableDefinition*> params) {
    assert(declNode->kind_ == AST::NodeKind::FUNCTION_DEFINITION ||
           declNode->kind_ == AST::NodeKind::EXTERNAL_FUNCTION_DECLARATION);

    for (const auto* param : params) {
        handleVariableDeclaration(param, param->identifier_->name_);
    }

    ensureSymbolUndeclaredOrError(declNode, name);

    const SymbolInfo info(declNode);
    functionsTable_.emplace(name, info);
}

void ConstraintGenerator::handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                                    const std::string_view name) {
    ensureSymbolUndeclaredOrError(declNode, name);

    const SymbolInfo info(declNode);
    scopedVariables_.emplace_back(name, info);
}

TypeID ConstraintGenerator::checkFunctionCall(const AST::FunctionCall& funcCall) {
    const std::string_view name = funcCall.callee_->name_;

    const auto info = getFunctionSymbolInfoOrError(name, *funcCall.callee_);
    if (!info) {
        return registerTypeVariable();
    }
    assert(info->kind() == SymbolKind::FUNCTION);

    const auto& params = info->parameters();

    if (funcCall.arguments_.size() != params.size()) {
        error(std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                          "got {}",
                          name, params.size(), funcCall.arguments_.size()),
              funcCall);
    }

    for (std::size_t i = 0; i < funcCall.arguments_.size() && i < params.size(); i++) {
        const TypeID argType = checkExpression(*funcCall.arguments_[i]);
        const TypeID paramType = params[i]->typeID_;

        equalityConstraint(argType, paramType, *funcCall.arguments_[i]);
    }

    return info->typeID();
}

TypeID ConstraintGenerator::checkUnaryExpression(const AST::UnaryExpression& unaryExpr) {
    const TypeID operandType = checkExpression(*unaryExpr.operand_);

    // TODO: perhaps remove trait constraints and manually check in Validator?

    const std::optional<Trait> requiredTrait = traitFromOperator(unaryExpr.operator_);
    if (requiredTrait) {
        traitConstraint(operandType, *requiredTrait, unaryExpr);
    }

    return operandType;
}

TypeID ConstraintGenerator::checkBinaryExpression(const AST::BinaryExpression& binaryExpr) {
    const TypeID leftType = checkExpression(*binaryExpr.left_);
    const TypeID rightType = checkExpression(*binaryExpr.right_);
    equalityConstraint(leftType, rightType, binaryExpr);

    // TODO: perhaps remove trait constraints and manually check in Validator?

    const AST::Operator op = binaryExpr.operator_;
    const std::optional<Trait> trait = traitFromOperator(op);
    if (trait) {
        traitConstraint(leftType, *trait, binaryExpr);
    }

    if (AST::isArithmeticOperator(op)) return leftType;
    if (AST::isComparisonOperator(op)) return registerBoolType();
    if (AST::isLogicalOperator(op)) return registerBoolType();
    std::unreachable();
}

__attribute__((cold)) void ConstraintGenerator::emptyArrayLiteralError(
    const AST::Expression& arrayLit) const {
    // TODO: perhaps emtpy array literals should be handled in the Validator instead? This requires
    //  handling the unknown type gracefully
    error("Array literal cannot be empty", arrayLit);
}

TypeID ConstraintGenerator::checkExpression(const AST::Expression& expr) {
    TypeID verifier;

    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL: {
            verifier = registerIntegerType();
            break;
        }
        case AST::NodeKind::BOOLEAN_LITERAL: {
            verifier = registerBoolType();
            break;
        }
        case AST::NodeKind::CHARACTER_LITERAL: {
            verifier = registerCharType();
            break;
        }
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = *expr.as<AST::ArrayLiteral>();
            if (arrayLiteral.elements_.empty()) {
                emptyArrayLiteralError(arrayLiteral);
                verifier = registerTypeVariable();
                break;
            }

            const TypeID elementTypeID = checkExpression(*arrayLiteral.elements_[0]);
            for (const auto* element : arrayLiteral.elements_ | std::views::drop(1)) {
                checkExpression(*element, elementTypeID);
            }

            verifier = typeManager_.createType(elementTypeID, arrayLiteral.elements_.size());
            break;
        }
        case AST::NodeKind::REPEAT_ARRAY_LITERAL: {
            const auto& repeatArrayLit = *expr.as<AST::RepeatArrayLiteral>();

            const auto elementCount = repeatArrayLit.count_->value_;
            if (elementCount == 0) {
                emptyArrayLiteralError(repeatArrayLit);
                verifier = registerTypeVariable();
                break;
            }

            const TypeID elementTypeID = checkExpression(*repeatArrayLit.element_);

            verifier = typeManager_.createType(elementTypeID, elementCount);
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = *expr.as<AST::Identifier>();
            const auto info = getVariableSymbolInfoOrError(identifier.name_, identifier);
            if (!info) {
                verifier = registerTypeVariable();
                break;
            } else if (info->kind() != SymbolKind::VARIABLE) {
                std::unreachable();
            }

            verifier = info->typeID();
            break;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<AST::ArrayAccess>();
            const TypeID arrayType = checkExpression(*arrayAccess.base_);

            traitConstraint(arrayType, Trait::SUBSCRIPT, arrayAccess);

            const TypeID integerTypeID = registerIntegerType();
            checkExpression(*arrayAccess.index_, integerTypeID);

            const TypeID elementTypeID = registerTypeVariable();
            subscriptConstraint(arrayType, elementTypeID, arrayAccess);

            verifier = elementTypeID;
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            verifier = checkFunctionCall(funcCall);
            break;
        }
        case AST::NodeKind::UNARY_EXPRESSION: {
            const auto& unaryExpr = *expr.as<AST::UnaryExpression>();
            verifier = checkUnaryExpression(unaryExpr);
            break;
        }
        case AST::NodeKind::BINARY_EXPRESSION: {
            const auto& binaryExpr = *expr.as<AST::BinaryExpression>();
            verifier = checkBinaryExpression(binaryExpr);
            break;
        }
        default:
            std::unreachable();
    }

    equalityConstraint(expr.typeID_, verifier, expr);
    return expr.typeID_;
}

void ConstraintGenerator::checkExpression(const AST::Expression& expr, const TypeID expected) {
    const TypeID exprType = checkExpression(expr);
    equalityConstraint(exprType, expected, expr);
}

void ConstraintGenerator::analyseVariableDefinition(const AST::VariableDefinition& definition) {
    const std::string_view name = definition.identifier_->name_;
    const TypeID assignedType = checkExpression(*definition.value_);

    equalityConstraint(definition.typeID_, assignedType, definition);
    storableConstraint(definition.typeID_, definition);

    handleVariableDeclaration(&definition, name);
}

bool ConstraintGenerator::verifyIsAssignable(const AST::Expression& expr) {
    // TODO: this should be moved to the Validator. However, this requires moving mutability
    //  information to the type system.

    switch (expr.kind_) {
        case AST::NodeKind::IDENTIFIER: {
            const std::string_view varName = expr.as<const AST::Identifier>()->name_;
            const auto& declarationInfo = getVariableSymbolInfoOrError(varName, expr);
            if (!declarationInfo) return false;

            assert(declarationInfo->kind() == SymbolKind::VARIABLE);
            if (!declarationInfo->isMutable()) {
                error(std::format("Assignment to immutable: `{}`", varName), expr);
                return false;
            }
            return true;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<const AST::ArrayAccess>();
            return verifyIsAssignable(*arrayAccess.base_);
        }
        default:
            error("Left-hand side of assignment is not a place expression", expr);
            return false;
    }
}

void ConstraintGenerator::analyseAssignment(const AST::Assignment& assignment) {
    const auto* place = assignment.place_;

    if (!verifyIsAssignable(*place)) {
        // If the place is not assignable, we cannot proceed further.
        // Just verify that the value expression is valid, then return.
        checkExpression(*assignment.value_);
        // TODO: can we not also check the place?
        return;
    }

    const TypeID placeType = checkExpression(*place);
    const TypeID valueType = checkExpression(*assignment.value_);
    equalityConstraint(placeType, valueType, assignment);

    if (assignment.operator_ != AST::Operator::ASSIGN) {
        const auto requiredTrait = traitFromOperator(assignment.operator_);
        assert(requiredTrait);
        traitConstraint(placeType, *requiredTrait, assignment);
    }
}

void ConstraintGenerator::analyseExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    checkExpression(*exprStmt.expression_);
}

void ConstraintGenerator::analyseIfStatement(const AST::IfStatement& ifStmt) {
    checkExpression(*ifStmt.condition_, registerBoolType());
    analyseStatement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyseStatement(*ifStmt.elseClause_);
    }
}

void ConstraintGenerator::analyseWhileStatement(const AST::WhileStatement& whileStmt) {
    checkExpression(*whileStmt.condition_, registerBoolType());
    analyseStatement(*whileStmt.body_);
}

void ConstraintGenerator::analyseReturnStatement(const AST::ReturnStatement& returnStmt) {
    TypeID returnTypeID;
    if (returnStmt.returnValue_) {
        returnTypeID = checkExpression(*returnStmt.returnValue_);
    } else {
        returnTypeID = registerVoidType();
    }
    equalityConstraint(returnTypeID, currentFunctionReturnTypeID_, returnStmt);
}

void ConstraintGenerator::analyseExit(const AST::ExitStatement& exitStmt) {
    checkExpression(*exitStmt.exitCode_, registerIntegerType());
}

void ConstraintGenerator::analyseStatement(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = *stmt.as<AST::VariableDefinition>();
            analyseVariableDefinition(varDecl);
            break;
        }
        case AST::NodeKind::ASSIGNMENT: {
            const auto& assignment = *stmt.as<AST::Assignment>();
            analyseAssignment(assignment);
            break;
        }
        case AST::NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = *stmt.as<AST::ExpressionStatement>();
            analyseExpressionStatement(exprStmt);
            break;
        }
        case AST::NodeKind::IF_STATEMENT: {
            const auto& ifStmt = *stmt.as<AST::IfStatement>();
            analyseIfStatement(ifStmt);
            break;
        }
        case AST::NodeKind::WHILE_STATEMENT: {
            const auto& whileStmt = *stmt.as<AST::WhileStatement>();
            analyseWhileStatement(whileStmt);
            break;
        }
        case AST::NodeKind::BREAK_STATEMENT:
        case AST::NodeKind::CONTINUE_STATEMENT:
            break;  // Nothing to check
        case AST::NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = *stmt.as<AST::ReturnStatement>();
            analyseReturnStatement(returnStmt);
            break;
        }
        case AST::NodeKind::EXIT_STATEMENT: {
            const auto& exitStmt = *stmt.as<AST::ExitStatement>();
            analyseExit(exitStmt);
            break;
        }
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = *stmt.as<AST::BlockStatement>();
            const ScopeGuard guard(*this);
            for (const auto* innerStmt : blockStmt.body_) {
                analyseStatement(*innerStmt);
            }
            break;
        }
        default:
            std::unreachable();
    }
}

void ConstraintGenerator::analyseExternalFunctionDeclaration(
    const AST::ExternalFunctionDeclaration& funcDecl) {
    const ScopeGuard guard(*this);
    handleFunctionDeclaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.parameters_);
}

void ConstraintGenerator::analyseFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    const ScopeGuard guard(*this);
    currentFunctionReturnTypeID_ = funcDef.returnTypeID_;

    handleFunctionDeclaration(&funcDef, funcDef.identifier_->name_, funcDef.parameters_);
    analyseStatement(*funcDef.body_);
}
