#include "SemanticAnalyser.hpp"

#include <cassert>
#include <format>
#include <functional>
#include <ranges>
#include <string>
#include <utility>

#include "SymbolTable.hpp"
#include "ast/Debug.hpp"
#include "type/TypeManager.hpp"
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
    for (const auto& funcDef : ast_->functions_) {
        if (funcDef->identifier_->name_ == ENTRY_POINT_NAME) {
            mainDef = funcDef.get();
            break;
        }
    }

    const auto mainIt = functionsTable_.find(std::string(ENTRY_POINT_NAME));
    if (targetType_ == TargetType::EXECUTABLE) {
        if (mainIt == functionsTable_.end() || mainIt->second.kind_ != SymbolKind::FUNCTION) {
            error(std::format("No `{}` function found in executable target", ENTRY_POINT_NAME),
                  *ast_);
        } else {
            assert(mainDef && "mainDef should not be null here");

            if (mainIt->second.parameters_.size() != 0) {
                error(std::format("`{}` function must not take any parameters", ENTRY_POINT_NAME),
                      *mainDef);
            }

            equalityConstraint(mainIt->second.typeID_, registerVoidType(), *mainDef);
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
    diagnosticsEngine_.emitErrors();
    exit(EXIT_FAILURE);
}

void SemanticAnalyser::error(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex());
}

void SemanticAnalyser::fatalError(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex());
    emitErrorsAndQuit();
}

void SemanticAnalyser::enterScope() { scopes_.emplace_back(); }

void SemanticAnalyser::exitScope() { scopes_.pop_back(); }

std::optional<const SymbolInfo*> SemanticAnalyser::getSymbolInfo(const std::string& name) const {
    {
        const auto it = functionsTable_.find(name);
        if (it != functionsTable_.end()) return &it->second;
    }

    for (auto const& scope : scopes_) {
        const auto it = scope.find(name);
        if (it != scope.end()) return &it->second;
    }

    return std::nullopt;
}

std::optional<const SymbolInfo*> SemanticAnalyser::getSymbolInfoOrError(
    const std::string& name, const AST::Node& node) const {
    const auto info = getSymbolInfo(name);
    if (!info.has_value()) {
        error(std::format("Undeclared symbol: `{}`", name), node);
    }
    return info;
}

SymbolInfo& SemanticAnalyser::declareSymbol(const AST::Node* declarationNode,
                                            const std::string& name, const SymbolKind kind,
                                            const bool isMutable, const TypeID typeID,
                                            const bool isScoped,
                                            std::vector<SymbolInfo> parameters) {
    if (getSymbolInfo(name).has_value()) {
        fatalError(std::format("Redeclaration of symbol: `{}`", name), *declarationNode);
    }

    SymbolInfo info{.name_ = name,
                    .kind_ = kind,
                    .isMutable_ = isMutable,
                    .typeID_ = typeID,
                    .parameters_ = std::move(parameters)};

    if (isScoped) {
        auto [it, _] = scopes_.back().emplace(name, std::move(info));
        return it->second;
    } else {
        assert(kind == SymbolKind::FUNCTION &&
               "Only functions can be declared in the global scope");
        auto [it, _] = functionsTable_.emplace(name, std::move(info));
        return it->second;
    }
}

SymbolInfo& SemanticAnalyser::handleFunctionDeclaration(
    const AST::Node* declNode, const std::string& name, const TypeID returnTypeID,
    const std::vector<std::unique_ptr<AST::VariableDefinition>>& params) {
    std::vector<SymbolInfo> parameterSymbols;
    for (const auto& param : params) {
        const TypeID paramType = param->typeID_;
        parameterSymbols.emplace_back(handleVariableDeclaration(
            param.get(), param->identifier_->name_, param->isMutable_, paramType));
    }

    return declareSymbol(declNode, name, SymbolKind::FUNCTION, false, returnTypeID, false,
                         parameterSymbols);
}

SymbolInfo& SemanticAnalyser::handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                                        const std::string& name,
                                                        const bool isMutable, const TypeID typeID) {
    return declareSymbol(declNode, name, SymbolKind::VARIABLE, isMutable, typeID, true, {});
}

TypeID SemanticAnalyser::checkFunctionCall(const AST::FunctionCall& funcCall) {
    const std::string& name = funcCall.callee_->name_;

    const auto info = getSymbolInfoOrError(name, *funcCall.callee_);
    if (!info.has_value()) {
        return registerAnyType();
    }

    if (info.value()->kind_ != SymbolKind::FUNCTION) {
        error(std::format("Attempted to call a non-function: `{}`", name), funcCall);
        return registerAnyType();
    }

    const auto& params = info.value()->parameters_;

    if (funcCall.arguments_.size() != params.size()) {
        error(std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                          "got {}",
                          name, params.size(), funcCall.arguments_.size()),
              funcCall);
    }

    for (std::size_t i = 0; i < funcCall.arguments_.size() && i < params.size(); i++) {
        const TypeID argType = checkExpression(*funcCall.arguments_[i]);
        const TypeID paramType = params[i].typeID_;

        equalityConstraint(argType, paramType, *funcCall.arguments_[i]);
    }

    return info.value()->typeID_;
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
            for (const auto& element : arrayLiteral.elements_ | std::views::drop(1)) {
                checkExpression(*element, elementTypeID);
            }

            verifier = typeManager_.createType(elementTypeID, arrayLiteral.elements_.size());
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = *expr.as<AST::Identifier>();
            const auto info = getSymbolInfoOrError(identifier.name_, identifier);
            if (!info.has_value()) {
                verifier = registerAnyType();
                break;
            } else if (info.value()->kind_ != SymbolKind::VARIABLE) {
                error(std::format("`{}` is not a variable", identifier.name_), identifier);
                verifier = registerAnyType();
                break;
            }

            verifier = info.value()->typeID_;
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
    const std::string& name = definition.identifier_->name_;
    const TypeID assignedType = checkExpression(*definition.value_);

    equalityConstraint(definition.typeID_, assignedType, definition);
    storableConstraint(definition.typeID_, definition);

    handleVariableDeclaration(&definition, name, definition.isMutable_, definition.typeID_);
}

bool SemanticAnalyser::verifyIsAssignable(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::IDENTIFIER: {
            const std::string& varName = expr.as<const AST::Identifier>()->name_;
            const auto& declarationInfo = getSymbolInfoOrError(varName, expr);
            if (!declarationInfo.has_value()) {
                return false;
            } else if (declarationInfo.value()->kind_ != SymbolKind::VARIABLE) {
                error(std::format("Assignment to non-variable: `{}`", varName), expr);
                return false;
            } else if (!declarationInfo.value()->isMutable_) {
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
    const auto& place = assignment.place_;

    if (!verifyIsAssignable(*place)) {
        // If the place is not assignable, we cannot proceed further.
        // Just verify that the value expression is valid then return.
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
            const TypeID returnTypeID = checkExpression(*returnStmt.returnValue_);
            equalityConstraint(returnTypeID, currentFunctionReturnTypeID_, returnStmt);
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
            for (const auto& innerStmt : blockStmt.body_) {
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
        for (const auto& innerStmt : blockStmt.body_) {
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
    handleFunctionDeclaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.returnTypeID_,
                              funcDecl.parameters_);
}

void SemanticAnalyser::analyseFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    const ScopeGuard guard(*this);
    currentFunctionName_ = funcDef.identifier_->name_;
    currentFunctionReturnTypeID_ = funcDef.returnTypeID_;

    const TypeID returnTypeID = currentFunctionReturnTypeID_;
    const Type& returnType = typeManager_.getType(returnTypeID);
    handleFunctionDeclaration(&funcDef, funcDef.identifier_->name_, returnTypeID,
                              funcDef.parameters_);

    analyseStatement(*funcDef.body_);

    const bool allPathsReturn = verifyStatementReturns(*funcDef.body_);
    if (!allPathsReturn && !returnType.isVoid()) {
        error(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        currentFunctionName_, returnType.toString(typeManager_)),
            funcDef);
    }

    currentFunctionName_.clear();
}
