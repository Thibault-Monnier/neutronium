#include "semantic-analysis/semantic_analyser.hpp"

#include <cassert>
#include <format>
#include <functional>
#include <string>
#include <utility>

#include "parsing/debug.hpp"
#include "semantic-analysis/types/TypeSolver.hpp"

void SemanticAnalyser::analyse() {
    for (const auto& externalFuncDecl : ast_->externalFunctions_) {
        analyse_external_function_declaration(*externalFuncDecl);
    }

    for (const auto& funcDef : ast_->functions_) {
        analyse_function_definition(*funcDef);
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
            abort("No `main` function found in executable target", *ast_);
        }

        if (mainIt->second.parameters_.size() != 0) {
            abort("`main` function must not take any parameters", *mainDef);
        }

        const TypeID voidTypeID = typeManager_.createType(PrimitiveKind::VOID);
        typeManager_.getTypeSolver().addConstraint(
            std::make_unique<EqualityConstraint>(mainIt->second.typeID_, voidTypeID, *mainDef));
    } else {
        if (mainIt != functionsTable_.end()) {
            abort("`main` function is not allowed in a library target", *mainDef);
        }
    }
}

void SemanticAnalyser::abort(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.report_error(errorMessage, node.source_start_index(),
                                    node.source_end_index());

    diagnosticsEngine_.emit_errors();
    exit(EXIT_FAILURE);
}

void SemanticAnalyser::enter_scope() { scopes_.emplace_back(); }

void SemanticAnalyser::exit_scope() { scopes_.pop_back(); }

std::optional<const SymbolInfo*> SemanticAnalyser::get_symbol_info(const std::string& name) const {
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

SymbolInfo& SemanticAnalyser::declare_symbol(const AST::Node* declarationNode,
                                             const std::string& name, const SymbolKind kind,
                                             const bool isMutable, const TypeID typeID,
                                             const bool isScoped,
                                             std::vector<SymbolInfo> parameters) {
    if (get_symbol_info(name).has_value()) {
        abort(std::format("Redeclaration of symbol: `{}`", name), *declarationNode);
    }

    SymbolInfo info{.name_ = name,
                    .kind_ = kind,
                    .isMutable_ = isMutable,
                    .typeID_ = typeID,
                    .parameters_ = std::move(parameters)};

    if (isScoped) {
        auto [it, _] = scopes_.back().emplace(name, std::move(info));
        return it->second;
    } else if (kind == SymbolKind::FUNCTION) {
        auto [it, _] = functionsTable_.emplace(name, std::move(info));
        return it->second;
    }

    std::unreachable();
}

SymbolInfo& SemanticAnalyser::handle_function_declaration(
    const AST::Node* declNode, const std::string& name, const TypeID returnTypeID,
    const std::vector<std::unique_ptr<AST::VariableDefinition>>& params) {
    std::vector<SymbolInfo> parameterSymbols;
    for (const auto& param : params) {
        const TypeID paramType = param->typeID_;
        parameterSymbols.emplace_back(handle_variable_declaration(
            param.get(), param->identifier_->name_, param->isMutable_, paramType));
    }

    return declare_symbol(declNode, name, SymbolKind::FUNCTION, false, returnTypeID, false,
                          parameterSymbols);
}

SymbolInfo& SemanticAnalyser::handle_variable_declaration(const AST::VariableDefinition* declNode,
                                                          const std::string& name,
                                                          const bool isMutable,
                                                          const TypeID typeID) {
    return declare_symbol(declNode, name, SymbolKind::VARIABLE, isMutable, typeID, true, {});
}

TypeID SemanticAnalyser::get_function_call_type(const AST::FunctionCall& funcCall) {
    const std::string& name = funcCall.callee_->name_;

    const auto info = get_symbol_info(name);
    if (!info.has_value()) {
        abort(std::format("Attempted to call undeclared function: `{}`", name), funcCall);
    }

    if (info.value()->kind_ != SymbolKind::FUNCTION) {
        abort(std::format("Attempted to call a non-function: `{}`", name), funcCall);
    }

    const auto& params = info.value()->parameters_;

    if (funcCall.arguments_.size() != params.size()) {
        abort(std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                          "got {}",
                          name, params.size(), funcCall.arguments_.size()),
              funcCall);
    }

    for (std::size_t i = 0; i < funcCall.arguments_.size(); i++) {
        const TypeID argType = get_expression_type(*funcCall.arguments_[i]);
        const TypeID paramType = params[i].typeID_;

        typeManager_.getTypeSolver().addConstraint(
            std::make_unique<EqualityConstraint>(argType, paramType, *funcCall.arguments_[i]));
    }

    return info.value()->typeID_;
}

TypeID SemanticAnalyser::get_unary_expression_type(const AST::UnaryExpression& unaryExpr) {
    const TypeID operandType = get_expression_type(*unaryExpr.operand_);

    if (unaryExpr.operator_ == AST::Operator::ADD ||
        unaryExpr.operator_ == AST::Operator::SUBTRACT) {
        typeManager_.getTypeSolver().addConstraint(std::make_unique<HasTraitConstraint>(
            operandType, trait_from_operator(unaryExpr.operator_).value(), unaryExpr));
        return operandType;
    }

    if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) {
        typeManager_.getTypeSolver().addConstraint(
            std::make_unique<HasTraitConstraint>(operandType, Trait::NOT, unaryExpr));
        return typeManager_.createType(PrimitiveKind::BOOL);
    }

    std::unreachable();
}

TypeID SemanticAnalyser::get_binary_expression_type(const AST::BinaryExpression& binaryExpr) {
    const TypeID leftType = get_expression_type(*binaryExpr.left_);
    const TypeID rightType = get_expression_type(*binaryExpr.right_);
    typeManager_.getTypeSolver().addConstraint(
        std::make_unique<EqualityConstraint>(leftType, rightType, binaryExpr));

    const AST::Operator op = binaryExpr.operator_;
    if (AST::is_arithmetic_operator(op)) {
        typeManager_.getTypeSolver().addConstraint(std::make_unique<HasTraitConstraint>(
            leftType, trait_from_operator(op).value(), binaryExpr));

        return leftType;
    }

    if (AST::is_equality_operator(op)) {
        typeManager_.getTypeSolver().addConstraint(
            std::make_unique<HasTraitConstraint>(leftType, Trait::EQ, binaryExpr));

        return typeManager_.createType(PrimitiveKind::BOOL);
    }

    if (AST::is_relational_operator(op)) {
        typeManager_.getTypeSolver().addConstraint(std::make_unique<HasTraitConstraint>(
            leftType, trait_from_operator(op).value(), binaryExpr));

        return typeManager_.createType(PrimitiveKind::BOOL);
    }

    std::unreachable();
}

TypeID SemanticAnalyser::get_expression_type(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return typeManager_.createType(Type::integerFamilyType());
        case AST::NodeKind::BOOLEAN_LITERAL:
            return typeManager_.createType(PrimitiveKind::BOOL);
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = static_cast<const AST::ArrayLiteral&>(expr);
            if (arrayLiteral.elements_.empty()) {
                abort("Array literal cannot be empty", arrayLiteral);
            }

            const TypeID elementTypeID = get_expression_type(*arrayLiteral.elements_[0]);
            for (const auto& element : arrayLiteral.elements_) {
                analyse_expression(*element, elementTypeID);
            }
            return typeManager_.createType(Type{typeManager_.getType(elementTypeID), elementTypeID,
                                                arrayLiteral.elements_.size()});
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            const auto info = get_symbol_info(identifier.name_);
            if (!info.has_value()) {
                abort(std::format("Attempted to access undeclared symbol: `{}`", identifier.name_),
                      identifier);
            } else if (info.value()->kind_ != SymbolKind::VARIABLE) {
                abort(std::format("`{}` is not a variable", identifier.name_), identifier);
            }

            return info.value()->typeID_;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(expr);
            const TypeID arrayType = get_expression_type(*arrayAccess.base_);

            typeManager_.getTypeSolver().addConstraint(
                std::make_unique<HasTraitConstraint>(arrayType, Trait::INDEX, *arrayAccess.base_));

            const TypeID integerTypeID = typeManager_.createType(Type::integerFamilyType());
            analyse_expression(*arrayAccess.index_, integerTypeID);

            return typeManager_.getType(arrayType).array_element_type_id();
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = static_cast<const AST::FunctionCall&>(expr);
            return get_function_call_type(funcCall);
        }
        case AST::NodeKind::UNARY_EXPRESSION: {
            const auto& unaryExpr = static_cast<const AST::UnaryExpression&>(expr);
            return get_unary_expression_type(unaryExpr);
        }
        case AST::NodeKind::BINARY_EXPRESSION: {
            const auto& binaryExpr = static_cast<const AST::BinaryExpression&>(expr);
            return get_binary_expression_type(binaryExpr);
        }
        default:
            std::unreachable();
    }
}

void SemanticAnalyser::analyse_expression(const AST::Expression& expr, const TypeID expected) {
    const TypeID exprType = get_expression_type(expr);
    typeManager_.getTypeSolver().addConstraint(
        std::make_unique<EqualityConstraint>(exprType, expected, expr));
}

void SemanticAnalyser::analyse_variable_definition(const AST::VariableDefinition& definition) {
    const std::string& name = definition.identifier_->name_;
    const TypeID assignedType = get_expression_type(*definition.value_);

    typeManager_.getTypeSolver().addConstraint(
        std::make_unique<EqualityConstraint>(definition.typeID_, assignedType, definition));

    handle_variable_declaration(&definition, name, definition.isMutable_, definition.typeID_);
}

void SemanticAnalyser::analyse_assignment(const AST::Assignment& assignment) {
    const auto& place = assignment.place_;

    std::function<void(const AST::Expression&)> verifyIsAssignable =
        [&](const AST::Expression& expr) {
            switch (expr.kind_) {
                case AST::NodeKind::IDENTIFIER: {
                    const std::string& varName = static_cast<const AST::Identifier&>(expr).name_;
                    const auto& declarationInfo = get_symbol_info(varName);
                    if (!declarationInfo.has_value()) {
                        abort(std::format("Assignment to undeclared variable: `{}`", varName),
                              expr);
                    } else if (declarationInfo.value()->kind_ != SymbolKind::VARIABLE) {
                        abort(std::format("Assignment to non-variable: `{}`", varName), expr);
                    } else if (!declarationInfo.value()->isMutable_) {
                        abort(std::format("Assignment to immutable: `{}`", varName), expr);
                    }
                    break;
                }
                case AST::NodeKind::ARRAY_ACCESS: {
                    const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(expr);
                    verifyIsAssignable(*arrayAccess.base_);
                    break;
                }
                default:
                    abort("Left-hand side of assignment must be a place expression, got " +
                              AST::node_kind_to_string(expr.kind_),
                          expr);
            }
        };

    verifyIsAssignable(*place);

    const TypeID placeType = get_expression_type(*place);
    const TypeID valueType = get_expression_type(*assignment.value_);
    typeManager_.getTypeSolver().addConstraint(
        std::make_unique<EqualityConstraint>(placeType, valueType, assignment));

    if (assignment.operator_ != AST::Operator::ASSIGN) {
        typeManager_.getTypeSolver().addConstraint(std::make_unique<HasTraitConstraint>(
            placeType, trait_from_operator(assignment.operator_).value(), assignment));
    }
}

void SemanticAnalyser::analyse_expression_statement(const AST::ExpressionStatement& exprStmt) {
    get_expression_type(*exprStmt.expression_);
}

void SemanticAnalyser::analyse_if_statement(const AST::IfStatement& ifStmt) {
    analyse_expression(*ifStmt.condition_, typeManager_.createType(PrimitiveKind::BOOL));
    analyse_statement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyse_statement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyse_while_statement(const AST::WhileStatement& whileStmt) {
    analyse_expression(*whileStmt.condition_, typeManager_.createType(PrimitiveKind::BOOL));
    loopDepth_++;
    analyse_statement(*whileStmt.body_);
    loopDepth_--;
}

void SemanticAnalyser::analyse_break_statement(const AST::BreakStatement& breakStmt) const {
    if (loopDepth_ == 0) {
        abort("`break` statement is not allowed outside of a loop", breakStmt);
    }
}

void SemanticAnalyser::analyse_continue_statement(
    const AST::ContinueStatement& continueStmt) const {
    if (loopDepth_ == 0) {
        abort("`continue` statement is not allowed outside of a loop", continueStmt);
    }
}

void SemanticAnalyser::analyse_exit(const AST::ExitStatement& exitStmt) {
    analyse_expression(*exitStmt.exitCode_, typeManager_.createType(Type::integerFamilyType()));
}

void SemanticAnalyser::analyse_statement(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = static_cast<const AST::VariableDefinition&>(stmt);
            analyse_variable_definition(varDecl);
            break;
        }
        case AST::NodeKind::ASSIGNMENT: {
            const auto& assignment = static_cast<const AST::Assignment&>(stmt);
            analyse_assignment(assignment);
            break;
        }
        case AST::NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = static_cast<const AST::ExpressionStatement&>(stmt);
            analyse_expression_statement(exprStmt);
            break;
        }
        case AST::NodeKind::IF_STATEMENT: {
            const auto& ifStmt = static_cast<const AST::IfStatement&>(stmt);
            analyse_if_statement(ifStmt);
            break;
        }
        case AST::NodeKind::WHILE_STATEMENT: {
            const auto& whileStmt = static_cast<const AST::WhileStatement&>(stmt);
            analyse_while_statement(whileStmt);
            break;
        }
        case AST::NodeKind::BREAK_STATEMENT:
            analyse_break_statement(static_cast<const AST::BreakStatement&>(stmt));
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            analyse_continue_statement(static_cast<const AST::ContinueStatement&>(stmt));
            break;
        case AST::NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = static_cast<const AST::ReturnStatement&>(stmt);
            const TypeID returnTypeID = get_expression_type(*returnStmt.returnValue_);
            typeManager_.getTypeSolver().addConstraint(std::make_unique<EqualityConstraint>(
                returnTypeID, currentFunctionReturnTypeID_, returnStmt));
            break;
        }
        case AST::NodeKind::EXIT_STATEMENT: {
            const auto& exitStmt = static_cast<const AST::ExitStatement&>(stmt);
            analyse_exit(exitStmt);
            break;
        }
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = static_cast<const AST::BlockStatement&>(stmt);
            enter_scope();
            for (const auto& innerStmt : blockStmt.body_) {
                analyse_statement(*innerStmt);
            }
            exit_scope();
            break;
        }
        default:
            std::unreachable();
    }
}

bool SemanticAnalyser::verify_statement_returns(const AST::Statement& stmt) {
    const AST::NodeKind kind = stmt.kind_;

    if (kind == AST::NodeKind::IF_STATEMENT) {
        const auto& ifStmt = static_cast<const AST::IfStatement&>(stmt);
        if (!ifStmt.elseClause_) return false;

        const bool body = verify_statement_returns(*ifStmt.body_);
        const bool elseBody = verify_statement_returns(*ifStmt.elseClause_);
        return body && elseBody;

    } else if (kind == AST::NodeKind::BLOCK_STATEMENT) {
        const auto& blockStmt = static_cast<const AST::BlockStatement&>(stmt);
        for (const auto& innerStmt : blockStmt.body_) {
            if (verify_statement_returns(*innerStmt)) return true;
        }

    } else if (kind == AST::NodeKind::RETURN_STATEMENT || kind == AST::NodeKind::EXIT_STATEMENT ||
               kind == AST::NodeKind::BREAK_STATEMENT) {
        return true;
    }

    return false;
}

void SemanticAnalyser::analyse_external_function_declaration(
    const AST::ExternalFunctionDeclaration& funcDecl) {
    enter_scope();
    handle_function_declaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.returnTypeID_,
                                funcDecl.parameters_);
    exit_scope();
}

void SemanticAnalyser::analyse_function_definition(const AST::FunctionDefinition& funcDef) {
    enter_scope();
    currentFunctionName_ = funcDef.identifier_->name_;
    currentFunctionReturnTypeID_ = funcDef.returnTypeID_;

    const TypeID returnTypeID = currentFunctionReturnTypeID_;
    const Type& returnType = typeManager_.getType(returnTypeID);
    handle_function_declaration(&funcDef, funcDef.identifier_->name_, returnTypeID,
                                funcDef.parameters_);

    analyse_statement(*funcDef.body_);

    const bool allPathsReturn = verify_statement_returns(*funcDef.body_);
    if (!allPathsReturn && !returnType.isVoid()) {
        abort(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        currentFunctionName_, returnType.to_string()),
            funcDef);
    }

    currentFunctionName_.clear();
    exit_scope();
}
