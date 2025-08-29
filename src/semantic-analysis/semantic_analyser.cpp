#include "semantic-analysis/semantic_analyser.hpp"

#include <cassert>
#include <format>
#include <functional>
#include <string>
#include <utility>

void SemanticAnalyser::analyse() {
    for (const auto& constDef : ast_->constants_) {
        analyse_constant_definition(*constDef);
    }

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

        if (mainIt->second.type_.mismatches(PrimitiveType::VOID)) {
            abort("`main` function must return `void`", *mainDef);
        } else if (mainIt->second.parameters_.size() != 0) {
            abort("`main` function must not take any parameters", *mainDef);
        }
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
                                             const bool isMutable, const Type& type,
                                             const bool isScoped,
                                             std::vector<SymbolInfo> parameters) {
    if (get_symbol_info(name).has_value()) {
        abort(std::format("Redeclaration of symbol: `{}`", name), *declarationNode);
    }

    SymbolInfo info{.name_ = name,
                    .kind_ = kind,
                    .isMutable_ = isMutable,
                    .type_ = type,
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

SymbolInfo& SemanticAnalyser::handle_constant_definition(const AST::ConstantDefinition* declNode,
                                                         const std::string& name,
                                                         const Type& type) {
    return declare_symbol(declNode, name, SymbolKind::CONSTANT, false, type, false, {});
}

SymbolInfo& SemanticAnalyser::handle_function_declaration(
    const AST::Node* declNode, const std::string& name, const Type& returnType,
    const std::vector<std::unique_ptr<AST::VariableDefinition>>& params) {
    std::vector<SymbolInfo> parameterSymbols;
    for (const auto& param : params) {
        parameterSymbols.emplace_back(handle_variable_declaration(
            param.get(), param->identifier_->name_, param->isMutable_, param->type_));
    }

    return declare_symbol(declNode, name, SymbolKind::FUNCTION, false, returnType, false,
                          parameterSymbols);
}

SymbolInfo& SemanticAnalyser::handle_variable_declaration(const AST::VariableDefinition* declNode,
                                                          const std::string& name,
                                                          const bool isMutable, const Type& type) {
    return declare_symbol(declNode, name, SymbolKind::VARIABLE, isMutable, type, true, {});
}

Type SemanticAnalyser::get_function_call_type(const AST::FunctionCall& funcCall) {
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
        const Type argType = get_expression_type(*funcCall.arguments_[i]);
        const Type& paramType = params[i].type_;
        if (argType.mismatches(paramType)) {
            abort(
                std::format("Function `{}` called with incorrect argument type for parameter `{}`: "
                            "expected {}, got {}",
                            name, params[i].name_, paramType.to_string(), argType.to_string()),
                *funcCall.arguments_[i]);
        }
    }

    return info.value()->type_;
}

Type SemanticAnalyser::get_unary_expression_type(const AST::UnaryExpression& unaryExpr) {
    const Type operandType = get_expression_type(*unaryExpr.operand_);
    if (operandType.matches(PrimitiveType::INTEGER)) {
        if (AST::is_arithmetic_operator(unaryExpr.operator_)) return PrimitiveType::INTEGER;

        abort("Invalid unary operator for integer operand: got " +
                  AST::operator_to_string(unaryExpr.operator_),
              unaryExpr);
    }
    if (operandType.matches(PrimitiveType::BOOLEAN)) {
        if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) return PrimitiveType::BOOLEAN;

        abort("Invalid unary operator for boolean operand: got " +
                  AST::operator_to_string(unaryExpr.operator_),
              unaryExpr);
    }

    abort("Invalid type for unary operation: expected integer or boolean, got " +
              operandType.to_string(),
          unaryExpr);
}

Type SemanticAnalyser::get_binary_expression_type(const AST::BinaryExpression& binaryExpr) {
    const Type leftType = get_expression_type(*binaryExpr.left_);
    const Type rightType = get_expression_type(*binaryExpr.right_);
    if (leftType.mismatches(rightType)) {
        abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                          leftType.to_string(), rightType.to_string()),
              binaryExpr);
    }

    const AST::Operator op = binaryExpr.operator_;
    if (AST::is_arithmetic_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER)) {
            abort("Invalid type for arithmetic operation: expected integer, got " +
                      leftType.to_string(),
                  binaryExpr);
        }
        return PrimitiveType::INTEGER;
    }

    if (AST::is_equality_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER) &&
            leftType.mismatches(PrimitiveType::BOOLEAN)) {
            abort("Invalid type for equality operation: expected integer or boolean, got " +
                      leftType.to_string(),
                  binaryExpr);
        }
        return PrimitiveType::BOOLEAN;
    }

    if (AST::is_relational_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER)) {
            abort("Invalid type for relational operation: expected integer, got " +
                      leftType.to_string(),
                  binaryExpr);
        }
        return PrimitiveType::BOOLEAN;
    }

    std::unreachable();
}

Type SemanticAnalyser::get_expression_type(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return PrimitiveType::INTEGER;
        case AST::NodeKind::BOOLEAN_LITERAL:
            return PrimitiveType::BOOLEAN;
        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = static_cast<const AST::ArrayLiteral&>(expr);
            if (arrayLiteral.elements_.empty()) {
                abort("Array literal cannot be empty", arrayLiteral);
            }

            const Type elementType = get_expression_type(*arrayLiteral.elements_[0]);
            for (const auto& element : arrayLiteral.elements_) {
                analyse_expression(*element, elementType, "array literal element");
            }
            return Type{elementType, arrayLiteral.elements_.size()};
        }
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            const auto info = get_symbol_info(identifier.name_);
            if (!info.has_value()) {
                abort(std::format("Attempted to access undeclared symbol: `{}`", identifier.name_),
                      identifier);
            } else if (info.value()->kind_ != SymbolKind::VARIABLE &&
                       info.value()->kind_ != SymbolKind::CONSTANT) {
                abort(std::format("`{}` is not a variable or constant", identifier.name_),
                      identifier);
            }
            return info.value()->type_;
        }
        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = static_cast<const AST::ArrayAccess&>(expr);
            const Type arrayType = get_expression_type(*arrayAccess.base_);
            if (arrayType.kind() != TypeKind::ARRAY) {
                abort(std::format("{} is indexed as an array", arrayType.to_string()), arrayAccess);
            }
            analyse_expression(*arrayAccess.index_, PrimitiveType::INTEGER, "array access index");
            return arrayType.array_element_type();
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

void SemanticAnalyser::analyse_expression(const AST::Expression& expr, const Type& expected,
                                          const std::string& location) {
    const Type exprType = get_expression_type(expr);
    if (exprType.mismatches(expected)) {
        abort(std::format("Invalid expression type for {}: expected {}, got {}", location,
                          expected.to_string(), exprType.to_string()),
              expr);
    }
}

void SemanticAnalyser::analyse_variable_definition(const AST::VariableDefinition& declaration) {
    const std::string& name = declaration.identifier_->name_;
    const Type variableType = get_expression_type(*declaration.value_);

    if (variableType.matches(PrimitiveType::ANY) || variableType.matches(PrimitiveType::VOID)) {
        abort(std::format("Invalid variable type: `{}` is declared as {}", name,
                          variableType.to_string()),
              declaration);
    }
    if (variableType.mismatches(declaration.type_, true)) {
        abort(std::format("Type mismatch: variable `{}` has {} type specifier, but has {} value",
                          name, declaration.type_.to_string(), variableType.to_string()),
              declaration);
    }

    handle_variable_declaration(&declaration, name, declaration.isMutable_, variableType);
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

    const Type placeType = get_expression_type(*place);
    const Type valueType = get_expression_type(*assignment.value_);
    if (placeType.mismatches(valueType)) {
        abort(std::format("Type mismatch in assignment: {} is assigned to {}",
                          placeType.to_string(), valueType.to_string()),
              assignment);
    }

    if (assignment.operator_ != AST::Operator::ASSIGN) {
        if (placeType.mismatches(PrimitiveType::INTEGER)) {
            abort(std::format("Invalid assignment operator: `{}` used for type {}",
                              AST::operator_to_string(assignment.operator_), placeType.to_string()),
                  assignment);
        }
    }
}

void SemanticAnalyser::analyse_expression_statement(const AST::ExpressionStatement& exprStmt) {
    get_expression_type(*exprStmt.expression_);
}

void SemanticAnalyser::analyse_if_statement(const AST::IfStatement& ifStmt) {
    analyse_expression(*ifStmt.condition_, PrimitiveType::BOOLEAN, "condition");
    analyse_statement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyse_statement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyse_while_statement(const AST::WhileStatement& whileStmt) {
    analyse_expression(*whileStmt.condition_, PrimitiveType::BOOLEAN, "condition");
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
    analyse_expression(*exitStmt.exitCode_, PrimitiveType::INTEGER, "exit code");
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
            const Type returnType = get_expression_type(*returnStmt.returnValue_);
            if (returnType.mismatches(currentFunctionReturnType_)) {
                abort(
                    std::format("Type mismatch in return statement: function `{}` expects a return "
                                "value of type {}, but got {} instead",
                                currentFunctionName_, currentFunctionReturnType_.to_string(),
                                returnType.to_string()),
                    returnStmt);
            }
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
    handle_function_declaration(&funcDecl, funcDecl.identifier_->name_, funcDecl.returnType_,
                                funcDecl.parameters_);
    exit_scope();
}

void SemanticAnalyser::analyse_function_definition(const AST::FunctionDefinition& funcDef) {
    enter_scope();
    currentFunctionName_ = funcDef.identifier_->name_;
    currentFunctionReturnType_ = funcDef.returnType_;

    handle_function_declaration(&funcDef, funcDef.identifier_->name_, funcDef.returnType_,
                                funcDef.parameters_);

    analyse_statement(*funcDef.body_);

    const bool allPathsReturn = verify_statement_returns(*funcDef.body_);
    if (!allPathsReturn && funcDef.returnType_.mismatches(PrimitiveType::VOID)) {
        abort(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        currentFunctionName_, funcDef.returnType_.to_string()),
            funcDef);
    }

    currentFunctionName_.clear();
    exit_scope();
}

void SemanticAnalyser::analyse_constant_definition(const AST::ConstantDefinition& declaration) {
    abort("Constants are not supported yet", declaration);
    const std::string& name = declaration.identifier_->name_;

    const Type constantType = get_expression_type(*declaration.value_);
    if (constantType.mismatches(PrimitiveType::INTEGER) &&
        constantType.mismatches(PrimitiveType::BOOLEAN)) {
        abort(std::format("Invalid constant type: `{}` is declared as {}", name,
                          constantType.to_string()),
              declaration);
    }
    if (constantType.mismatches(declaration.type_)) {
        abort(std::format("Type mismatch: constant `{}` has {} type specifier, but has {} value",
                          name, declaration.type_.to_string(), constantType.to_string()),
              declaration);
    }

    handle_constant_definition(&declaration, name, constantType);
}
