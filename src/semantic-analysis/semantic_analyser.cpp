#include "semantic-analysis/semantic_analyser.hpp"

#include <format>
#include <iostream>
#include <stdexcept>
#include <string>

#include "utils/log.hpp"

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

    const auto mainIt = functionsTable_.find("main");
    if (targetType_ == TargetType::EXECUTABLE) {
        if (mainIt == functionsTable_.end() || mainIt->second.kind_ != SymbolKind::FUNCTION) {
            abort("No `main` function found in executable target");
        } else if (!mainIt->second.type_.matches(PrimitiveType::VOID)) {
            abort("`main` function must return `void`");
        } else if (mainIt->second.parameters_.size() != 0) {
            abort("`main` function must not take any parameters");
        }
    } else {
        if (mainIt != functionsTable_.end()) {
            abort("`main` function is not allowed in a library target");
        }
    }
}

void SemanticAnalyser::abort(const std::string& errorMessage) {
    print_error(errorMessage);
    exit(EXIT_FAILURE);
}

void SemanticAnalyser::enter_scope() { scopes_.emplace_back(); }

void SemanticAnalyser::exit_scope() { scopes_.pop_back(); }

std::optional<const SymbolInfo*> SemanticAnalyser::get_symbol_info(const std::string& name) const {
    {
        auto it = functionsTable_.find(name);
        if (it != functionsTable_.end()) return &it->second;
    }

    for (auto const& scope : scopes_) {
        auto it = scope.find(name);
        if (it != scope.end()) return &it->second;
    }

    return std::nullopt;
}

SymbolInfo& SemanticAnalyser::declare_symbol(const std::string& name, const SymbolKind kind,
                                             const bool isMutable, const Type& type,
                                             const bool isScoped,
                                             std::vector<SymbolInfo> parameters) {
    if (get_symbol_info(name).has_value()) {
        abort(std::format("Redeclaration of symbol: `{}`", name));
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

    throw std::invalid_argument("Invalid symbol kind for non-scoped symbol: " +
                                std::to_string(static_cast<int>(kind)));
}

SymbolInfo& SemanticAnalyser::handle_constant_definition(const std::string& name,
                                                         const Type& type) {
    return declare_symbol(name, SymbolKind::CONSTANT, false, type, false, {});
}

SymbolInfo& SemanticAnalyser::handle_function_declaration(
    const std::string& name, const Type& returnType,
    const std::vector<std::unique_ptr<AST::VariableDefinition>>& params) {
    std::vector<SymbolInfo> parameterSymbols;
    for (const auto& param : params) {
        parameterSymbols.emplace_back(handle_variable_declaration(param->identifier_->name_,
                                                                  param->isMutable_, param->type_));
    }

    return declare_symbol(name, SymbolKind::FUNCTION, false, returnType, false, parameterSymbols);
}

SymbolInfo& SemanticAnalyser::handle_variable_declaration(const std::string& name,
                                                          const bool isMutable, const Type& type) {
    return declare_symbol(name, SymbolKind::VARIABLE, isMutable, type, true, {});
}

Type SemanticAnalyser::get_function_call_type(  // NOLINT(*-no-recursion)
    const AST::FunctionCall& funcCall) {
    const std::string& name = funcCall.identifier_->name_;

    const auto info = get_symbol_info(name);
    if (!info.has_value()) {
        abort(std::format("Attempted to call undeclared function: `{}`", name));
    }

    if (info.value()->kind_ != SymbolKind::FUNCTION) {
        abort(std::format("Attempted to call a non-function: `{}`", name));
    }

    const auto& params = info.value()->parameters_;

    if (funcCall.arguments_.size() != params.size()) {
        abort(
            std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                        "got {}",
                        name, params.size(), funcCall.arguments_.size()));
    }

    for (std::size_t i = 0; i < funcCall.arguments_.size(); i++) {
        const Type argType = get_expression_type(*funcCall.arguments_[i]);
        const Type& paramType = params[i].type_;
        if (argType.mismatches(paramType)) {
            abort(
                std::format("Function `{}` called with incorrect argument type for parameter `{}`: "
                            "expected {}, got {}",
                            name, params[i].name_, paramType.to_string(), argType.to_string()));
        }
    }

    return info.value()->type_;
}

Type SemanticAnalyser::get_unary_expression_type(  // NOLINT(*-no-recursion)
    const AST::UnaryExpression& unaryExpr) {
    const Type operandType = get_expression_type(*unaryExpr.operand_);
    if (operandType.matches(PrimitiveType::INTEGER)) {
        if (AST::is_arithmetic_operator(unaryExpr.operator_)) return PrimitiveType::INTEGER;

        abort("Invalid unary operator for integer operand: got " +
              AST::operator_to_string(unaryExpr.operator_));
    }
    if (operandType.matches(PrimitiveType::BOOLEAN)) {
        if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) return PrimitiveType::BOOLEAN;

        abort("Invalid unary operator for boolean operand: got " +
              AST::operator_to_string(unaryExpr.operator_));
    }

    abort("Invalid type for unary operation: expected integer or boolean, got " +
          operandType.to_string());
}

Type SemanticAnalyser::get_binary_expression_type(  // NOLINT(*-no-recursion)
    const AST::BinaryExpression& binaryExpr) {
    const Type leftType = get_expression_type(*binaryExpr.left_);
    const Type rightType = get_expression_type(*binaryExpr.right_);
    if (leftType.mismatches(rightType)) {
        abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                          leftType.to_string(), rightType.to_string()));
    }

    const AST::Operator op = binaryExpr.operator_;
    if (AST::is_arithmetic_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER)) {
            abort("Invalid type for arithmetic operation: expected integer, got " +
                  leftType.to_string());
        }
        return PrimitiveType::INTEGER;
    }

    if (AST::is_equality_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER) &&
            leftType.mismatches(PrimitiveType::BOOLEAN)) {
            abort("Invalid type for equality operation: expected integer or boolean, got " +
                  leftType.to_string());
        }
        return PrimitiveType::BOOLEAN;
    }

    if (AST::is_relational_operator(op)) {
        if (leftType.mismatches(PrimitiveType::INTEGER)) {
            abort("Invalid type for relational operation: expected integer, got " +
                  leftType.to_string());
        }
        return PrimitiveType::BOOLEAN;
    }

    throw std::invalid_argument("Invalid operator in binary expression");
}

Type SemanticAnalyser::get_expression_type(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return PrimitiveType::INTEGER;
        case AST::NodeKind::BOOLEAN_LITERAL:
            return PrimitiveType::BOOLEAN;
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            const auto info = get_symbol_info(identifier.name_);
            if (!info.has_value()) {
                abort(std::format("Attempted to access undeclared symbol: `{}`", identifier.name_));
            } else if (info.value()->kind_ != SymbolKind::VARIABLE &&
                       info.value()->kind_ != SymbolKind::CONSTANT) {
                abort(std::format("`{}` is not a variable or constant", identifier.name_));
            }
            return info.value()->type_;
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
            throw std::invalid_argument("Invalid expression kind");
    }
}

void SemanticAnalyser::analyse_expression(const AST::Expression& expr, const Type& expected,
                                          const std::string& location) {
    const Type exprType = get_expression_type(expr);
    if (exprType.mismatches(expected)) {
        abort(std::format("Invalid expression type for {}: expected {}, got {}", location,
                          expected.to_string(), exprType.to_string()));
    }
}

void SemanticAnalyser::analyse_variable_definition(const AST::VariableDefinition& declaration) {
    const std::string& name = declaration.identifier_->name_;
    const Type variableType = get_expression_type(*declaration.value_);

    if (variableType.mismatches(PrimitiveType::INTEGER) &&
        variableType.mismatches(PrimitiveType::BOOLEAN)) {
        abort(std::format("Invalid variable type: `{}` is declared as {}", name,
                          variableType.to_string()));
    }
    if (variableType.mismatches(declaration.type_)) {
        abort(std::format("Type mismatch: variable `{}` has {} type specifier, but has {} value",
                          name, declaration.type_.to_string(), variableType.to_string()));
    }

    handle_variable_declaration(name, declaration.isMutable_, variableType);
}

void SemanticAnalyser::analyse_variable_assignment(const AST::Assignment& assignment) {
    // const std::string& name = assignment.identifier_->name_;
    // const auto& declarationInfo = get_symbol_info(name);
    // if (!declarationInfo.has_value()) {
    //     abort(std::format("Assignment to undeclared variable: `{}`", name));
    // }
    //
    // if (declarationInfo.value()->kind_ != SymbolKind::VARIABLE) {
    //     abort(std::format("Assignment to non-variable: `{}`", name));
    // }
    // if (!declarationInfo.value()->isMutable_) {
    //     abort(std::format("Assignment to immutable variable: `{}`", name));
    // }
    //
    // const Type assignmentType = get_expression_type(*assignment.value_);
    // const Type& declarationType = declarationInfo.value()->type_;
    // if (assignmentType.mismatches(declarationType)) {
    //     abort(std::format("Type mismatch: variable `{}` is declared as {}, but assigned to {}",
    //                       name, declarationType.to_string(), assignmentType.to_string()));
    // }
}

void SemanticAnalyser::analyse_expression_statement(  // NOLINT(*-no-recursion)
    const AST::ExpressionStatement& exprStmt) {
    analyse_expression(*exprStmt.expression_, PrimitiveType::ANY, "expression statement");
}

void SemanticAnalyser::analyse_if_statement(  // NOLINT(*-no-recursion)
    const AST::IfStatement& ifStmt) {
    analyse_expression(*ifStmt.condition_, PrimitiveType::BOOLEAN, "condition");
    analyse_statement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyse_statement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyse_while_statement(  // NOLINT(*-no-recursion)
    const AST::WhileStatement& whileStmt) {
    analyse_expression(*whileStmt.condition_, PrimitiveType::BOOLEAN, "condition");
    loopDepth_++;
    analyse_statement(*whileStmt.body_);
    loopDepth_--;
}

void SemanticAnalyser::analyse_break_statement() const {
    if (loopDepth_ == 0) {
        abort("`break` statement is not allowed outside of a loop");
    }
}

void SemanticAnalyser::analyse_continue_statement() const {
    if (loopDepth_ == 0) {
        abort("`continue` statement is not allowed outside of a loop");
    }
}

void SemanticAnalyser::analyse_exit(const AST::ExitStatement& exitStmt) {
    analyse_expression(*exitStmt.exitCode_, PrimitiveType::INTEGER, "exit code");
}

void SemanticAnalyser::analyse_statement(const AST::Statement& stmt) {  // NOLINT(*-no-recursion)
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = static_cast<const AST::VariableDefinition&>(stmt);
            analyse_variable_definition(varDecl);
            break;
        }
        case AST::NodeKind::ASSIGNMENT: {
            const auto& assignment = static_cast<const AST::Assignment&>(stmt);
            analyse_variable_assignment(assignment);
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
            analyse_break_statement();
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            analyse_continue_statement();
            break;
        case AST::NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = static_cast<const AST::ReturnStatement&>(stmt);
            const Type returnType = get_expression_type(*returnStmt.returnValue_);
            if (returnType.mismatches(currentFunctionReturnType_)) {
                abort(
                    std::format("Type mismatch in return statement: function `{}` expects a return "
                                "value of type {}, but got {} instead",
                                currentFunctionName_, currentFunctionReturnType_.to_string(),
                                returnType.to_string()));
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
            throw std::invalid_argument("Invalid statement kind at semantic analysis");
    }
}

bool SemanticAnalyser::verify_statement_returns(  // NOLINT(*-no-recursion)
    const AST::Statement& stmt) {
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
    handle_function_declaration(funcDecl.identifier_->name_, funcDecl.returnType_,
                                funcDecl.parameters_);
    exit_scope();
}

void SemanticAnalyser::analyse_function_definition(  // NOLINT(*-no-recursion)
    const AST::FunctionDefinition& funcDef) {
    enter_scope();
    currentFunctionName_ = funcDef.identifier_->name_;
    currentFunctionReturnType_ = funcDef.returnType_;

    handle_function_declaration(funcDef.identifier_->name_, funcDef.returnType_,
                                funcDef.parameters_);

    analyse_statement(*funcDef.body_);

    const bool allPathsReturn = verify_statement_returns(*funcDef.body_);
    if (!allPathsReturn && funcDef.returnType_.mismatches(PrimitiveType::VOID)) {
        abort(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        currentFunctionName_, funcDef.returnType_.to_string()));
    }

    currentFunctionName_.clear();
    exit_scope();
}

void SemanticAnalyser::analyse_constant_definition(const AST::ConstantDefinition& declaration) {
    abort("Constants are not supported yet");
    const std::string& name = declaration.identifier_->name_;

    const Type constantType = get_expression_type(*declaration.value_);
    if (constantType.mismatches(PrimitiveType::INTEGER) &&
        constantType.mismatches(PrimitiveType::BOOLEAN)) {
        abort(std::format("Invalid constant type: `{}` is declared as {}", name,
                          constantType.to_string()));
    }
    if (constantType.mismatches(declaration.type_)) {
        abort(std::format("Type mismatch: constant `{}` has {} type specifier, but has {} value",
                          name, declaration.type_.to_string(), constantType.to_string()));
    }

    handle_constant_definition(name, constantType);
}
