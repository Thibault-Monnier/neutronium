#include "SemanticAnalyser.hpp"

#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "driver/Cli.hpp"
#include "type/Trait.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"
#include "type/inference/Constraint.hpp"
#include "type/inference/TypeSolver.hpp"

void SemanticAnalyser::equalityConstraint(TypeID a, TypeID b, const AST::Node& node) const {
    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(a, b, node);
}

void SemanticAnalyser::traitConstraint(TypeID type, Trait trait, const AST::Node& node) const {
    typeManager_.getTypeSolver().addConstraint<HasTraitConstraint>(type, trait, node);
}

void SemanticAnalyser::subscriptConstraint(TypeID arrayType, TypeID elementType,
                                           const AST::Node& node) const {
    typeManager_.getTypeSolver().addConstraint<SubscriptConstraint>(arrayType, elementType, node);
}

void SemanticAnalyser::storableConstraint(TypeID type, const AST::Node& node) const {
    typeManager_.getTypeSolver().addConstraint<StorableConstraint>(type, node);
}

void SemanticAnalyser::analyse() {
    for (const auto& externalFuncDecl : ast_->externalFunctions_) {
        analyseExternalFunctionDeclaration(*externalFuncDecl);
    }

    for (const auto& funcDef : ast_->functions_) {
        analyseFunctionDefinition(*funcDef);
    }

    const AST::FunctionDefinition* mainDef = nullptr;
    for (const auto* funcDef : ast_->functions_) {
        if (funcDef->identifier_->name_ == ENTRY_POINT_NAME) {
            mainDef = funcDef;
            break;
        }
    }

    const auto mainIt = functionsTable_.find(std::string(ENTRY_POINT_NAME));
    if (targetType_ == TargetType::EXECUTABLE) {
        if (mainIt == functionsTable_.end() || mainIt->second.kind() != SymbolKind::FUNCTION) {
            error(std::format("No `{}` function found in executable target", ENTRY_POINT_NAME),
                  *ast_);
        } else {
            assert(mainDef && "mainDef should not be null here");

            if (mainIt->second.parameters().size() != 0) {
                error(std::format("`{}` function must not take any parameters", ENTRY_POINT_NAME),
                      *mainDef);
            }

            equalityConstraint(mainIt->second.typeID(), registerVoidType(), *mainDef);
        }
    } else {
        if (mainIt != functionsTable_.end()) {
            assert(mainDef && "mainDef should not be null here");
            error(std::format("`{}` function is not allowed in a library target", ENTRY_POINT_NAME),
                  *mainDef);
        }
    }

    if (diagnosticsEngine_.hasErrors()) {
        emitErrorsAndQuit();
    }

    typeManager_.getTypeSolver().solve();
}

void SemanticAnalyser::emitErrorsAndQuit() const {
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}

void SemanticAnalyser::error(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex(),
                                   fileID_);
}

void SemanticAnalyser::fatalError(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex(),
                                   fileID_);
    emitErrorsAndQuit();
}

void SemanticAnalyser::enterScope() { scopes_.emplace_back(); }

void SemanticAnalyser::exitScope() { scopes_.pop_back(); }

__attribute__((cold)) void SemanticAnalyser::handleUndeclaredSymbolError(
    std::string_view name, const AST::Node& node, [[maybe_unused]] const SymbolKind kind) const {
    const auto info = getSymbolInfo(name);
    if (info.has_value()) {
        assert(info.value()->kind() != kind);
        switch (info.value()->kind()) {
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

std::optional<const SymbolInfo*> SemanticAnalyser::getFunctionSymbolInfo(
    const std::string_view name) const {
    const auto it = functionsTable_.find(name);
    if (it != functionsTable_.end()) return &it->second;
    return std::nullopt;
}

std::optional<const SymbolInfo*> SemanticAnalyser::getVariableSymbolInfo(
    const std::string_view name) const {
    // Innermost scopes have the highest chance of containing the symbol
    for (const auto& scope : std::ranges::reverse_view(scopes_)) {
        const auto it = scope.find(name);
        if (it != scope.end()) return &it->second;
    }

    return std::nullopt;
}

std::optional<const SymbolInfo*> SemanticAnalyser::getSymbolInfo(
    const std::string_view name) const {
    const auto varInfo = getVariableSymbolInfo(name);
    if (varInfo.has_value()) return varInfo;

    const auto funcInfo = getFunctionSymbolInfo(name);
    if (funcInfo.has_value()) return funcInfo;

    return std::nullopt;
}

std::optional<const SymbolInfo*> SemanticAnalyser::getFunctionSymbolInfoOrError(
    const std::string_view name, const AST::Node& node) const {
    const auto info = getFunctionSymbolInfo(name);

    if (!info.has_value()) handleUndeclaredSymbolError(name, node, SymbolKind::FUNCTION);
    return info;
}

std::optional<const SymbolInfo*> SemanticAnalyser::getVariableSymbolInfoOrError(
    const std::string_view name, const AST::Node& node) const {
    const auto info = getVariableSymbolInfo(name);

    if (!info.has_value()) handleUndeclaredSymbolError(name, node, SymbolKind::VARIABLE);
    return info;
}

void SemanticAnalyser::symbolUndeclaredOrError(const AST::Node* declarationNode,
                                               const std::string_view name) const {
    if (getSymbolInfo(name).has_value()) {
        fatalError(std::format("Redeclaration of symbol: `{}`", name), *declarationNode);
    }
}

SymbolInfo& SemanticAnalyser::handleFunctionDeclaration(
    const AST::Node* declNode, const std::string_view name,
    const std::span<AST::VariableDefinition*> params) {
    assert(declNode->kind_ == AST::NodeKind::FUNCTION_DEFINITION ||
           declNode->kind_ == AST::NodeKind::EXTERNAL_FUNCTION_DECLARATION);

    for (const auto* param : params) {
        handleVariableDeclaration(param, param->identifier_->name_);
    }

    symbolUndeclaredOrError(declNode, name);

    const SymbolInfo info(declNode);
    auto [it, _] = functionsTable_.emplace(name, info);
    return it->second;
}

SymbolInfo& SemanticAnalyser::handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                                        const std::string_view name) {
    symbolUndeclaredOrError(declNode, name);

    const SymbolInfo info(declNode);
    auto [it, _] = scopes_.back().emplace(name, info);
    return it->second;
}

TypeID SemanticAnalyser::checkFunctionCall(const AST::FunctionCall& funcCall) {
    const std::string_view name = funcCall.callee_->name_;

    const auto info = getFunctionSymbolInfoOrError(name, *funcCall.callee_);
    if (!info.has_value()) {
        return registerAnyType();
    }

    if (info.value()->kind() != SymbolKind::FUNCTION) {
        std::unreachable();
        return registerAnyType();
    }

    const auto& params = info.value()->parameters();

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

    return info.value()->typeID();
}

TypeID SemanticAnalyser::checkUnaryExpression(const AST::UnaryExpression& unaryExpr) {
    const TypeID operandType = checkExpression(*unaryExpr.operand_);

    const std::optional<Trait> requiredTrait = traitFromOperator(unaryExpr.operator_);
    if (requiredTrait.has_value()) {
        traitConstraint(operandType, requiredTrait.value(), unaryExpr);
    }

    return operandType;
}

TypeID SemanticAnalyser::checkBinaryExpression(const AST::BinaryExpression& binaryExpr) {
    const TypeID leftType = checkExpression(*binaryExpr.left_);
    const TypeID rightType = checkExpression(*binaryExpr.right_);
    equalityConstraint(leftType, rightType, binaryExpr);

    const AST::Operator op = binaryExpr.operator_;
    const std::optional<Trait> trait = traitFromOperator(op);
    if (trait.has_value()) {
        traitConstraint(leftType, trait.value(), binaryExpr);
    }

    if (AST::isArithmeticOperator(op)) return leftType;
    if (AST::isEqualityOperator(op) || AST::isRelationalOperator(op)) return registerBoolType();
    std::unreachable();
}

TypeID SemanticAnalyser::checkExpression(const AST::Expression& expr) {
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
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = *expr.as<AST::ArrayLiteral>();
            if (arrayLiteral.elements_.empty()) {
                error("Array literal cannot be empty", arrayLiteral);
                verifier = registerAnyType();
                break;
            }

            const TypeID elementTypeID = checkExpression(*arrayLiteral.elements_[0]);
            for (const auto* element : arrayLiteral.elements_ | std::views::drop(1)) {
                checkExpression(*element, elementTypeID);
            }

            verifier = typeManager_.createType(elementTypeID, arrayLiteral.elements_.size());
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = *expr.as<AST::Identifier>();
            const auto info = getVariableSymbolInfoOrError(identifier.name_, identifier);
            if (!info.has_value()) {
                verifier = registerAnyType();
                break;
            } else if (info.value()->kind() != SymbolKind::VARIABLE) {
                std::unreachable();
            }

            verifier = info.value()->typeID();
            break;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<AST::ArrayAccess>();
            const TypeID arrayType = checkExpression(*arrayAccess.base_);

            traitConstraint(arrayType, Trait::SUBSCRIPT, arrayAccess);

            const TypeID integerTypeID = registerIntegerType();
            checkExpression(*arrayAccess.index_, integerTypeID);

            const TypeID elementTypeID = registerAnyType();
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

void SemanticAnalyser::checkExpression(const AST::Expression& expr, const TypeID expected) {
    const TypeID exprType = checkExpression(expr);
    equalityConstraint(exprType, expected, expr);
}

void SemanticAnalyser::analyseVariableDefinition(const AST::VariableDefinition& definition) {
    const std::string_view name = definition.identifier_->name_;
    const TypeID assignedType = checkExpression(*definition.value_);

    equalityConstraint(definition.typeID_, assignedType, definition);
    storableConstraint(definition.typeID_, definition);

    handleVariableDeclaration(&definition, name);
}

bool SemanticAnalyser::verifyIsAssignable(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::IDENTIFIER: {
            const std::string_view varName = expr.as<const AST::Identifier>()->name_;
            const auto& declarationInfo = getVariableSymbolInfoOrError(varName, expr);
            if (!declarationInfo.has_value()) return false;

            assert(declarationInfo.value()->kind() == SymbolKind::VARIABLE);
            if (!declarationInfo.value()->isMutable()) {
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

void SemanticAnalyser::analyseAssignment(const AST::Assignment& assignment) {
    const auto* place = assignment.place_;

    if (!verifyIsAssignable(*place)) {
        // If the place is not assignable, we cannot proceed further.
        // Just verify that the value expression is valid, then return.
        checkExpression(*assignment.value_);
        return;
    }

    const TypeID placeType = checkExpression(*place);
    const TypeID valueType = checkExpression(*assignment.value_);
    equalityConstraint(placeType, valueType, assignment);

    if (assignment.operator_ != AST::Operator::ASSIGN) {
        traitConstraint(placeType, traitFromOperator(assignment.operator_).value(), assignment);
    }
}

void SemanticAnalyser::analyseExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    checkExpression(*exprStmt.expression_);
}

void SemanticAnalyser::analyseIfStatement(const AST::IfStatement& ifStmt) {
    checkExpression(*ifStmt.condition_, registerBoolType());
    analyseStatement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyseStatement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyseWhileStatement(const AST::WhileStatement& whileStmt) {
    checkExpression(*whileStmt.condition_, registerBoolType());
    loopDepth_++;
    analyseStatement(*whileStmt.body_);
    loopDepth_--;
}

void SemanticAnalyser::analyseBreakStatement(const AST::BreakStatement& breakStmt) const {
    if (loopDepth_ == 0) {
        error("`break` statement is not allowed outside of a loop", breakStmt);
    }
}

void SemanticAnalyser::analyseContinueStatement(const AST::ContinueStatement& continueStmt) const {
    if (loopDepth_ == 0) {
        error("`continue` statement is not allowed outside of a loop", continueStmt);
    }
}

void SemanticAnalyser::analyseReturnStatement(const AST::ReturnStatement& returnStmt) {
    TypeID returnTypeID;
    if (returnStmt.returnValue_) {
        returnTypeID = checkExpression(*returnStmt.returnValue_);
    } else {
        returnTypeID = registerVoidType();
    }
    equalityConstraint(returnTypeID, currentFunctionReturnTypeID_, returnStmt);
}

void SemanticAnalyser::analyseExit(const AST::ExitStatement& exitStmt) {
    checkExpression(*exitStmt.exitCode_, registerIntegerType());
}

void SemanticAnalyser::analyseStatement(const AST::Statement& stmt) {
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
            analyseBreakStatement(*stmt.as<AST::BreakStatement>());
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            analyseContinueStatement(*stmt.as<AST::ContinueStatement>());
            break;
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

bool SemanticAnalyser::verifyStatementReturns(const AST::Statement& stmt) {
    const AST::NodeKind kind = stmt.kind_;

    if (kind == AST::NodeKind::IF_STATEMENT) {
        const auto& ifStmt = *stmt.as<AST::IfStatement>();
        if (!ifStmt.elseClause_) return false;

        const bool body = verifyStatementReturns(*ifStmt.body_);
        const bool elseBody = verifyStatementReturns(*ifStmt.elseClause_);
        return body && elseBody;

    } else if (kind == AST::NodeKind::BLOCK_STATEMENT) {
        const auto& blockStmt = *stmt.as<AST::BlockStatement>();
        for (const auto* innerStmt : blockStmt.body_) {
            if (verifyStatementReturns(*innerStmt)) return true;
        }

    } else if (kind == AST::NodeKind::RETURN_STATEMENT || kind == AST::NodeKind::EXIT_STATEMENT ||
               kind == AST::NodeKind::BREAK_STATEMENT) {
        return true;
    }

    return false;
}

void SemanticAnalyser::analyseExternalFunctionDeclaration(
    const AST::ExternalFunctionDeclaration& funcDecl) {
    const ScopeGuard guard(*this);
    handleFunctionDeclaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.parameters_);
}

void SemanticAnalyser::analyseFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    const ScopeGuard guard(*this);
    currentFunctionName_ = funcDef.identifier_->name_;
    currentFunctionReturnTypeID_ = funcDef.returnTypeID_;

    const TypeID returnTypeID = currentFunctionReturnTypeID_;
    const Type& returnType = typeManager_.getType(returnTypeID);
    handleFunctionDeclaration(&funcDef, funcDef.identifier_->name_, funcDef.parameters_);

    analyseStatement(*funcDef.body_);

    const bool allPathsReturn = verifyStatementReturns(*funcDef.body_);
    if (!allPathsReturn && !returnType.isVoid()) {
        error(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        currentFunctionName_, returnType.toString(typeManager_)),
            funcDef);
    }

    currentFunctionName_ = "";
}
