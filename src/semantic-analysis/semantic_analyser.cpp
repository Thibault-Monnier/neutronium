#include "semantic-analysis/semantic_analyser.hpp"

#include <format>
#include <iostream>
#include <stdexcept>
#include <string>

#include "utils/log.hpp"

SymbolTable SemanticAnalyser::analyse() {
    analyse_statement(*ast_->body_);

    for (const auto& [name, info] : symbolTable_) {
        std::cout << "Variable: " << name << ", Type: " << info.type_.to_string()
                  << ", Kind: " << symbol_kind_to_string(info.kind_)
                  << ", Is Mutable: " << info.isMutable_
                  << ", Stack Offset: " << info.stackOffset_.value_or(-1) << "\n";
    }

    std::cout << "\033[1;32mAnalysis completed successfully.\033[0m\n";

    return std::move(symbolTable_);
}

void SemanticAnalyser::abort(const std::string& errorMessage, const std::string& hintMessage) {
    print_error(errorMessage);
    if (!hintMessage.empty()) {
        print_hint(hintMessage);
    }
    exit(EXIT_FAILURE);
}

void SemanticAnalyser::enter_scope() { scopes_.emplace_back(); }

void SemanticAnalyser::exit_scope() {
    currentStackOffset_ -= scopes_.back().frameSize_;
    scopes_.pop_back();
}

bool SemanticAnalyser::is_symbol_declared_in_scope(const std::string& name) const {
    for (const Scope& scope : scopes_) {
        if (scope.symbols_.contains(name)) {
            return true;
        }
    }
    return false;
}

Type SemanticAnalyser::get_symbol_type(const std::string& name) const {
    return symbolTable_.at(name).type_;
}

SymbolKind SemanticAnalyser::get_symbol_kind(const std::string& name) const {
    return symbolTable_.at(name).kind_;
}

SymbolInfo& SemanticAnalyser::declare_symbol(const std::string& name, const SymbolKind kind,
                                             const bool isMutable, const Type type,
                                             const AST::Node& declarationNode,
                                             std::vector<SymbolInfo> parameters) {
    if (symbolTable_.contains(name)) {
        abort(std::format("Redeclaration of symbol: `{}`", name),
              "Shadowing is not permitted, even for disjoint scopes");
    }

    scopes_.back().symbols_.insert(name);

    std::optional<int> stackOffset;
    if (kind == SymbolKind::VARIABLE) {
        constexpr int VAR_STACK_SIZE = 8;
        stackOffset = currentStackOffset_;
        scopes_.back().variablesStackOffset_[name] = *stackOffset;
        scopes_.back().frameSize_ += VAR_STACK_SIZE;
        currentStackOffset_ += VAR_STACK_SIZE;
    }

    SymbolInfo info{.name_ = name,
                    .kind_ = kind,
                    .isMutable_ = isMutable,
                    .type_ = type,
                    .declarationNode_ = &declarationNode,
                    .stackOffset_ = stackOffset,
                    .parameters_ = std::move(parameters)};

    auto [it, _] = symbolTable_.emplace(name, std::move(info));
    return it->second;
}

SymbolInfo& SemanticAnalyser::handle_variable_declaration(const std::string& name,
                                                          const bool isMutable, const Type type,
                                                          const AST::Node& declarationNode) {
    return declare_symbol(name, SymbolKind::VARIABLE, isMutable, type, declarationNode, {});
}

SymbolInfo& SemanticAnalyser::handle_function_declaration(
    const std::string& name, const Type returnType, const std::vector<SymbolInfo>& parameterSymbols,
    const AST::Node& declarationNode) {
    return declare_symbol(name, SymbolKind::FUNCTION, false, returnType, declarationNode,
                          parameterSymbols);
}

Type SemanticAnalyser::get_function_call_type(  // NOLINT(*-no-recursion)
    const AST::FunctionCall& funcCall) {
    const std::string& name = funcCall.identifier_->name_;
    if (!is_symbol_declared_in_scope(name)) {
        abort(std::format("Attempted to call undeclared function: `{}`", name));
    }
    const SymbolInfo& info = symbolTable_.at(name);
    if (info.kind_ != SymbolKind::FUNCTION) {
        abort(std::format("Attempted to call a non-function: `{}`", name));
    }

    const auto& funcDecl = static_cast<const AST::FunctionDeclaration&>(*info.declarationNode_);

    if (funcCall.arguments_.size() != funcDecl.parameters_.size()) {
        abort(
            std::format("Function `{}` called with incorrect number of arguments: expected {}, "
                        "got {}",
                        name, funcDecl.parameters_.size(), funcCall.arguments_.size()));
    }
    for (uint i = 0; i < funcCall.arguments_.size(); i++) {
        const Type argType = get_expression_type(*funcCall.arguments_[i]);
        const Type paramType = funcDecl.parameters_[i]->type_;
        if (!argType.matches(paramType)) {
            abort(
                std::format("Function `{}` called with incorrect argument type for parameter `{}`: "
                            "expected {}, got {}",
                            name, funcDecl.parameters_[i]->identifier_->name_,
                            paramType.to_string(), argType.to_string()));
        }
    }

    return info.type_;
}

Type SemanticAnalyser::get_unary_expression_type(  // NOLINT(*-no-recursion)
    const AST::UnaryExpression& unaryExpr) {
    const Type operandType = get_expression_type(*unaryExpr.operand_);
    if (operandType.raw() == RawType::INTEGER) {
        if (AST::is_arithmetic_operator(unaryExpr.operator_)) return RawType::INTEGER;

        abort("Invalid unary operator for integer operand: got " +
              AST::operator_to_string(unaryExpr.operator_));
    }
    if (operandType.raw() == RawType::BOOLEAN) {
        if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) return RawType::BOOLEAN;

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
    if (leftType.raw() != rightType.raw()) {
        abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                          leftType.to_string(), rightType.to_string()));
    }

    const AST::Operator op = binaryExpr.operator_;
    if (AST::is_arithmetic_operator(op)) {
        if (leftType.raw() != RawType::INTEGER) {
            abort("Invalid type for arithmetic operation: expected integer, got " +
                  leftType.to_string());
        }
        return RawType::INTEGER;
    }

    if (AST::is_equality_operator(op)) {
        if (leftType.raw() != RawType::INTEGER && leftType.raw() != RawType::BOOLEAN) {
            abort("Invalid type for equality operation: expected integer or boolean, got " +
                  leftType.to_string());
        }
        return RawType::BOOLEAN;
    }

    if (AST::is_relational_operator(op)) {
        if (leftType.raw() != RawType::INTEGER) {
            abort("Invalid type for relational operation: expected integer, got " +
                  leftType.to_string());
        }
        return RawType::BOOLEAN;
    }

    throw std::invalid_argument("Invalid operator in binary expression");
}

Type SemanticAnalyser::get_expression_type(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return RawType::INTEGER;
        case AST::NodeKind::BOOLEAN_LITERAL:
            return RawType::BOOLEAN;
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            if (!is_symbol_declared_in_scope(identifier.name_)) {
                abort(
                    std::format("Attempted to access undeclared variable: `{}`", identifier.name_));
            } else if (get_symbol_kind(identifier.name_) != SymbolKind::VARIABLE) {
                abort(std::format("`{}` is not a variable", identifier.name_));
            }
            return get_symbol_type(identifier.name_);
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

void SemanticAnalyser::analyse_expression(const AST::Expression& expr, const Type expected,
                                          const std::string& location) {
    const Type exprType = get_expression_type(expr);
    if (!exprType.matches(expected)) {
        abort(std::format("Invalid expression type for {}: expected {}, got {}", location,
                          expected.to_string(), exprType.to_string()));
    }
}

void SemanticAnalyser::analyse_variable_declaration(const AST::VariableDeclaration& declaration) {
    const std::string& name = declaration.identifier_->name_;
    const Type variableType = get_expression_type(*declaration.value_);

    if (variableType.raw() != RawType::INTEGER && variableType.raw() != RawType::BOOLEAN) {
        abort(std::format("Invalid variable type: `{}` is declared as {}", name,
                          variableType.to_string()));
    }
    if (!variableType.matches(declaration.type_)) {
        abort(std::format("Type mismatch: variable `{}` has {} type specifier, but has {} value",
                          name, declaration.type_.to_string(), variableType.to_string()));
    }

    handle_variable_declaration(name, declaration.isMutable_, variableType, declaration);
}

void SemanticAnalyser::analyse_variable_assignment(const AST::VariableAssignment& assignment) {
    const std::string& name = assignment.identifier_->name_;
    if (!is_symbol_declared_in_scope(name)) {
        abort(std::format("Assignment to undeclared variable: `{}`", name));
    }

    const SymbolInfo& declaredSymbol = symbolTable_.at(name);
    if (declaredSymbol.kind_ != SymbolKind::VARIABLE) {
        abort(std::format("Assignment to non-variable: `{}`", name));
    }
    if (!declaredSymbol.isMutable_) {
        abort(std::format("Assignment to immutable variable: `{}`", name));
    }

    const Type assignmentType = get_expression_type(*assignment.value_);
    if (assignmentType.raw() != declaredSymbol.type_.raw()) {
        abort(std::format("Type mismatch: variable `{}` is declared as {}, but assigned to {}",
                          name, declaredSymbol.type_.to_string(), assignmentType.to_string()));
    }
}

void SemanticAnalyser::analyse_expression_statement(  // NOLINT(*-no-recursion)
    const AST::ExpressionStatement& exprStmt) {
    analyse_expression(*exprStmt.expression_, RawType::ANY, "expression statement");
}

void SemanticAnalyser::analyse_if_statement(  // NOLINT(*-no-recursion)
    const AST::IfStatement& ifStmt) {
    analyse_expression(*ifStmt.condition_, RawType::BOOLEAN, "condition");
    analyse_statement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyse_statement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyse_while_statement(  // NOLINT(*-no-recursion)
    const AST::WhileStatement& whileStmt) {
    analyse_expression(*whileStmt.condition_, RawType::BOOLEAN, "condition");
    loopDepth_++;
    analyse_statement(*whileStmt.body_);
    loopDepth_--;
}

void SemanticAnalyser::analyse_function_declaration(  // NOLINT(*-no-recursion)
    const AST::FunctionDeclaration& funcDecl) {
    enter_scope();
    std::vector<SymbolInfo> parameterSymbols;
    for (const auto& param : funcDecl.parameters_) {
        parameterSymbols.emplace_back(handle_variable_declaration(
            param->identifier_->name_, param->isMutable_, param->type_, *param));
    }
    analyse_statement(*funcDecl.body_);
    exit_scope();

    const std::string& name = funcDecl.identifier_->name_;
    handle_function_declaration(name, RawType::VOID, parameterSymbols, funcDecl);
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

void SemanticAnalyser::analyse_exit(const AST::Exit& exitStmt) {
    analyse_expression(*exitStmt.exitCode_, RawType::INTEGER, "exit code");
}

void SemanticAnalyser::analyse_statement(const AST::Statement& stmt) {  // NOLINT(*-no-recursion)
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DECLARATION: {
            const auto& varDecl = static_cast<const AST::VariableDeclaration&>(stmt);
            analyse_variable_declaration(varDecl);
            break;
        }
        case AST::NodeKind::VARIABLE_ASSIGNMENT: {
            const auto& assignment = static_cast<const AST::VariableAssignment&>(stmt);
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
        case AST::NodeKind::FUNCTION_DECLARATION: {
            const auto& funcDecl = static_cast<const AST::FunctionDeclaration&>(stmt);
            analyse_function_declaration(funcDecl);
            break;
        }
        case AST::NodeKind::BREAK_STATEMENT:
            analyse_break_statement();
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            analyse_continue_statement();
            break;
        case AST::NodeKind::EXIT: {
            const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
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
