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
#include "type/TypeSolver.hpp"

void SemanticAnalyser::analyse() {
    for (const auto& externalFuncDecl : ast_->externalFunctions_) {
        analyseExternalFunctionDeclaration(*externalFuncDecl);
    }

    for (const auto& funcDef : ast_->functions_) {
        analyseFunctionDefinition(*funcDef);
    }

    const AST::FunctionDefinition* mainDef = nullptr;
    for (const auto& funcDef : ast_->functions_) {
        if (funcDef->identifier_->name_ == "main") {
            mainDef = funcDef.get();
            break;
        }
    }

    const auto mainIt = functionsTable_.find("main");
    if (targetType_ == TargetType::EXECUTABLE) {
        if (mainIt == functionsTable_.end() || mainIt->second.kind_ != SymbolKind::FUNCTION) {
            error("No `main` function found in executable target", *ast_);
        } else {
            assert(mainDef && "mainDef should not be null here");

            if (mainIt->second.parameters_.size() != 0) {
                error("`main` function must not take any parameters", *mainDef);
            }

            const TypeID voidTypeID = typeManager_.createType(Primitive::Kind::VOID);
            typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(mainIt->second.typeID_,
                                                                           voidTypeID, *mainDef);
        }
    } else {
        if (mainIt != functionsTable_.end()) {
            assert(mainDef && "mainDef should not be null here");
            error("`main` function is not allowed in a library target", *mainDef);
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

TypeID SemanticAnalyser::getFunctionCallType(const AST::FunctionCall& funcCall) {
    const std::string& name = funcCall.callee_->name_;

    const auto info = getSymbolInfo(name);
    if (!info.has_value()) {
        error(std::format("Attempted to call undeclared function: `{}`", name), funcCall);
        return typeManager_.createType(Type::anyFamilyType());
    }

    if (info.value()->kind_ != SymbolKind::FUNCTION) {
        error(std::format("Attempted to call a non-function: `{}`", name), funcCall);
        return typeManager_.createType(Type::anyFamilyType());
    }

    const auto& params = info.value()->parameters_;

    if (funcCall.arguments_.size() != params.size()) {
        error(std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                          "got {}",
                          name, params.size(), funcCall.arguments_.size()),
              funcCall);
    }

    for (std::size_t i = 0; i < funcCall.arguments_.size() && i < params.size(); i++) {
        const TypeID argType = getExpressionType(*funcCall.arguments_[i]);
        const TypeID paramType = params[i].typeID_;

        typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(argType, paramType,
                                                                       *funcCall.arguments_[i]);
    }

    return info.value()->typeID_;
}

TypeID SemanticAnalyser::getUnaryExpressionType(const AST::UnaryExpression& unaryExpr) {
    const TypeID operandType = getExpressionType(*unaryExpr.operand_);

    const std::optional<Trait> requiredTrait = traitFromOperator(unaryExpr.operator_);

    if (requiredTrait.has_value()) {
        typeManager_.getTypeSolver().addConstraint<HasTraitConstraint>(
            operandType, requiredTrait.value(), unaryExpr);
    }

    switch (unaryExpr.operator_) {
        case AST::Operator::ADD:
        case AST::Operator::SUBTRACT:
            return operandType;
        case AST::Operator::LOGICAL_NOT:
            return typeManager_.createType(Primitive::Kind::BOOL);
        default:
            std::unreachable();
    }
}

TypeID SemanticAnalyser::getBinaryExpressionType(const AST::BinaryExpression& binaryExpr) {
    const TypeID leftType = getExpressionType(*binaryExpr.left_);
    const TypeID rightType = getExpressionType(*binaryExpr.right_);
    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(leftType, rightType, binaryExpr);

    const AST::Operator op = binaryExpr.operator_;
    const std::optional<Trait> trait = traitFromOperator(op);
    if (trait.has_value()) {
        typeManager_.getTypeSolver().addConstraint<HasTraitConstraint>(
            leftType, traitFromOperator(op).value(), binaryExpr);
    }

    if (AST::isArithmeticOperator(op)) return leftType;
    if (AST::isEqualityOperator(op) || AST::isRelationalOperator(op))
        return typeManager_.createType(Primitive::Kind::BOOL);
    std::unreachable();
}

TypeID SemanticAnalyser::getExpressionType(const AST::Expression& expr) {
    TypeID verifier;

    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL: {
            verifier = typeManager_.createType(Type::integerFamilyType());
            break;
        }
        case AST::NodeKind::BOOLEAN_LITERAL: {
            verifier = typeManager_.createType(Primitive::Kind::BOOL);
            break;
        }
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = *expr.as<AST::ArrayLiteral>();
            if (arrayLiteral.elements_.empty()) {
                error("Array literal cannot be empty", arrayLiteral);
                verifier = typeManager_.createType(Type::anyFamilyType());
                break;
            }

            const TypeID elementTypeID = getExpressionType(*arrayLiteral.elements_[0]);
            for (const auto& element : arrayLiteral.elements_ | std::views::drop(1)) {
                analyseExpression(*element, elementTypeID);
            }

            verifier = typeManager_.createType(elementTypeID, arrayLiteral.elements_.size());
            break;
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = *expr.as<AST::Identifier>();
            const auto info = getSymbolInfo(identifier.name_);
            if (!info.has_value()) {
                error(std::format("Attempted to access undeclared symbol: `{}`", identifier.name_),
                      identifier);
                verifier = typeManager_.createType(Type::anyFamilyType());
                break;
            } else if (info.value()->kind_ != SymbolKind::VARIABLE) {
                error(std::format("`{}` is not a variable", identifier.name_), identifier);
                verifier = typeManager_.createType(Type::anyFamilyType());
                break;
            }

            verifier = info.value()->typeID_;
            break;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<AST::ArrayAccess>();
            const TypeID arrayType = getExpressionType(*arrayAccess.base_);

            typeManager_.getTypeSolver().addConstraint<HasTraitConstraint>(
                arrayType, Trait::SUBSCRIPT, arrayAccess);

            const TypeID integerTypeID = typeManager_.createType(Type::integerFamilyType());
            analyseExpression(*arrayAccess.index_, integerTypeID);

            const TypeID elementTypeID = typeManager_.createType(Type::anyFamilyType());
            typeManager_.getTypeSolver().addConstraint<SubscriptConstraint>(
                arrayType, elementTypeID, arrayAccess);

            verifier = elementTypeID;
            break;
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            verifier = getFunctionCallType(funcCall);
            break;
        }
        case AST::NodeKind::UNARY_EXPRESSION: {
            const auto& unaryExpr = *expr.as<AST::UnaryExpression>();
            verifier = getUnaryExpressionType(unaryExpr);
            break;
        }
        case AST::NodeKind::BINARY_EXPRESSION: {
            const auto& binaryExpr = *expr.as<AST::BinaryExpression>();
            verifier = getBinaryExpressionType(binaryExpr);
            break;
        }
        default:
            std::unreachable();
    }

    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(expr.typeID_, verifier, expr);
    return expr.typeID_;
}

void SemanticAnalyser::analyseExpression(const AST::Expression& expr, const TypeID expected) {
    const TypeID exprType = getExpressionType(expr);
    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(exprType, expected, expr);
}

void SemanticAnalyser::analyseVariableDefinition(const AST::VariableDefinition& definition) {
    const std::string& name = definition.identifier_->name_;
    const TypeID assignedType = getExpressionType(*definition.value_);

    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(definition.typeID_, assignedType,
                                                                   definition);
    typeManager_.getTypeSolver().addConstraint<StorableConstraint>(definition.typeID_, definition);

    handleVariableDeclaration(&definition, name, definition.isMutable_, definition.typeID_);
}

void SemanticAnalyser::analyseAssignment(const AST::Assignment& assignment) {
    const auto& place = assignment.place_;

    std::function<bool(const AST::Expression&)> verifyIsAssignable =
        [&](const AST::Expression& expr) {
            switch (expr.kind_) {
                case AST::NodeKind::IDENTIFIER: {
                    const std::string& varName = expr.as<const AST::Identifier>()->name_;
                    const auto& declarationInfo = getSymbolInfo(varName);
                    if (!declarationInfo.has_value()) {
                        error(std::format("Assignment to undeclared variable: `{}`", varName),
                              expr);
                        return false;
                    } else if (declarationInfo.value()->kind_ != SymbolKind::VARIABLE) {
                        error(std::format("Assignment to non-variable: `{}`", varName), expr);
                        return false;
                    } else if (!declarationInfo.value()->isMutable_) {
                        error(std::format("Assignment to immutable: `{}`", varName), expr);
                        return false;
                    }
                    break;
                }
                case AST::NodeKind::ARRAY_ACCESS: {
                    const auto& arrayAccess = *expr.as<const AST::ArrayAccess>();
                    if (!verifyIsAssignable(*arrayAccess.base_)) return false;
                    break;
                }
                default:
                    error("Left-hand side of assignment is not a place expression", expr);
                    return false;
            }
            return true;
        };

    if (!verifyIsAssignable(*place)) {
        // If the place is not assignable, we cannot proceed further.
        // Just verify that the value expression is valid then return.
        getExpressionType(*assignment.value_);
        return;
    }

    const TypeID placeType = getExpressionType(*place);
    const TypeID valueType = getExpressionType(*assignment.value_);
    typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(placeType, valueType,
                                                                   assignment);

    if (assignment.operator_ != AST::Operator::ASSIGN) {
        typeManager_.getTypeSolver().addConstraint<HasTraitConstraint>(
            placeType, traitFromOperator(assignment.operator_).value(), assignment);
    }
}

void SemanticAnalyser::analyseExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    getExpressionType(*exprStmt.expression_);
}

void SemanticAnalyser::analyseIfStatement(const AST::IfStatement& ifStmt) {
    analyseExpression(*ifStmt.condition_, typeManager_.createType(Primitive::Kind::BOOL));
    analyseStatement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyseStatement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyseWhileStatement(const AST::WhileStatement& whileStmt) {
    analyseExpression(*whileStmt.condition_, typeManager_.createType(Primitive::Kind::BOOL));
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
    analyseExpression(*exitStmt.exitCode_, typeManager_.createType(Type::integerFamilyType()));
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
            const TypeID returnTypeID = getExpressionType(*returnStmt.returnValue_);
            typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(
                returnTypeID, currentFunctionReturnTypeID_, returnStmt);
            break;
        }
        case AST::NodeKind::EXIT_STATEMENT: {
            const auto& exitStmt = *stmt.as<AST::ExitStatement>();
            analyseExit(exitStmt);
            break;
        }
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = *stmt.as<AST::BlockStatement>();
            enterScope();
            for (const auto& innerStmt : blockStmt.body_) {
                analyseStatement(*innerStmt);
            }
            exitScope();
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
    enterScope();
    handleFunctionDeclaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.returnTypeID_,
                              funcDecl.parameters_);
    exitScope();
}

void SemanticAnalyser::analyseFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    enterScope();
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
    exitScope();
}
