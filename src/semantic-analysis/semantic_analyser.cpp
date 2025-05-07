#include "semantic-analysis/semantic_analyser.hpp"

#include <format>
#include <iostream>
#include <stdexcept>
#include <string>

#include "utils/log.hpp"

SymbolTable SemanticAnalyser::analyse() {
    analyse_statement(*ast_->body_);

    for (const auto& [name, info] : symbolTable_) {
        std::cout << "Variable: " << name << ", Type: " << type_to_string(info.type_)
                  << ", Kind: " << symbol_kind_to_string(info.kind_) << "\n";
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

void SemanticAnalyser::enter_scope(const AST::BlockStatement& blockStmt) {
    Scope scope;
    int frameSize = 0;
    for (const auto& stmt : blockStmt.body_) {
        if (stmt->kind_ == AST::NodeKind::ASSIGNMENT) {
            const auto& assignment = static_cast<AST::Assignment&>(*stmt);
            if (assignment.isDeclaration_) {
                frameSize += 8;
                scope.variablesStackOffset_[assignment.identifier_->name_] =
                    currentStackOffset_ + frameSize;
            }
        }
    }

    scope.frameSize_ = (frameSize + 15) & ~15;
    currentStackOffset_ += scope.frameSize_;
    scopes_.push_back(scope);
}

void SemanticAnalyser::exit_scope() {
    currentStackOffset_ -= scopes_.back().frameSize_;
    scopes_.pop_back();
}

bool SemanticAnalyser::is_symbol_declared(const std::string& name) {
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

void SemanticAnalyser::handle_symbol_declaration(const std::string& name, const Type type,
                                                 const SymbolKind kind,
                                                 const AST::Node& declarationNode) {
    if (is_symbol_declared(name)) {
        abort(std::format("Redeclaration of symbol: `{}`", name),
              "Shadowing is not permitted, even for disjoint scopes");
    }

    scopes_.back().symbols_.emplace(name);

    SymbolInfo info{
        .kind_ = kind,
        .type_ = type,
        .declarationNode_ = &declarationNode,
        .stackOffset_ = (kind == SymbolKind::VARIABLE)
                            ? std::optional(scopes_.back().variablesStackOffset_.at(name))
                            : std::nullopt,
    };
    symbolTable_.emplace(name, std::move(info));
}

Type SemanticAnalyser::get_unary_expression_type(  // NOLINT(*-no-recursion)
    const AST::UnaryExpression& unaryExpr) {
    const Type operandType = get_expression_type(*unaryExpr.operand_);
    if (operandType == Type::INTEGER) {
        if (AST::is_arithmetic_operator(unaryExpr.operator_)) return Type::INTEGER;

        abort("Invalid unary operator for integer operand: got " +
              AST::operator_to_string(unaryExpr.operator_));
    }
    if (operandType == Type::BOOLEAN) {
        if (unaryExpr.operator_ == AST::Operator::LOGICAL_NOT) return Type::BOOLEAN;

        abort("Invalid unary operator for boolean operand: got " +
              AST::operator_to_string(unaryExpr.operator_));
    }

    abort("Invalid type for unary operation: expected integer or boolean, got " +
          type_to_string(operandType));
}

Type SemanticAnalyser::get_binary_expression_type(  // NOLINT(*-no-recursion)
    const AST::BinaryExpression& binaryExpr) {
    const Type leftType = get_expression_type(*binaryExpr.left_);
    const Type rightType = get_expression_type(*binaryExpr.right_);
    if (leftType != rightType) {
        abort(std::format("Type mismatch in binary expression: left is {}, right is {}",
                          type_to_string(leftType), type_to_string(rightType)));
    }

    const AST::Operator op = binaryExpr.operator_;
    if (AST::is_arithmetic_operator(op)) {
        if (leftType != Type::INTEGER) {
            abort("Invalid type for arithmetic operation: expected integer, got " +
                  type_to_string(leftType));
        }
        return Type::INTEGER;
    }

    if (AST::is_equality_operator(op)) {
        if (leftType != Type::INTEGER && leftType != Type::BOOLEAN) {
            abort("Invalid type for equality operation: expected integer or boolean, got " +
                  type_to_string(leftType));
        }
        return Type::BOOLEAN;
    }

    if (AST::is_relational_operator(op)) {
        if (leftType != Type::INTEGER) {
            abort("Invalid type for relational operation: expected integer, got " +
                  type_to_string(leftType));
        }
        return Type::BOOLEAN;
    }

    throw std::invalid_argument("Invalid operator in binary expression");
}

Type SemanticAnalyser::get_expression_type(const AST::Expression& expr) {  // NOLINT(*-no-recursion)
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
            return Type::INTEGER;
        case AST::NodeKind::BOOLEAN_LITERAL:
            return Type::BOOLEAN;
        case AST::NodeKind::IDENTIFIER: {
            const auto& identifier = static_cast<const AST::Identifier&>(expr);
            if (!is_symbol_declared(identifier.name_)) {
                abort(
                    std::format("Attempted to access undeclared variable: `{}`", identifier.name_));
            } else if (get_symbol_kind(identifier.name_) != SymbolKind::VARIABLE) {
                abort(std::format("`{}` is not a variable", identifier.name_));
            }
            return get_symbol_type(identifier.name_);
        }
        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = static_cast<const AST::FunctionCall&>(expr);
            const std::string& name = funcCall.identifier_->name_;
            if (!is_symbol_declared(name)) {
                abort(std::format("Attempted to call undeclared function: `{}`", name));
            }
            const SymbolInfo& info = symbolTable_.at(name);
            if (info.kind_ != SymbolKind::FUNCTION) {
                abort(std::format("Attempted to call a non-function: `{}`", name));
            }
            return info.type_;
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
    const Type type = get_expression_type(expr);
    if (type != expected) {
        abort(std::format("Invalid expression type for {}: expected {}, got {}", location,
                          type_to_string(expected), type_to_string(type)));
    }
}

void SemanticAnalyser::analyse_declaration_assignment(const AST::Assignment& assignment) {
    const std::string& name = assignment.identifier_->name_;
    if (symbolTable_.contains(name)) {
        abort(std::format("Redeclaration of symbol: `{}`", name),
              "Shadowing is not permitted, even for disjoint scopes");
    }

    const Type variableType = get_expression_type(*assignment.value_);
    if (variableType != Type::INTEGER && variableType != Type::BOOLEAN) {
        abort(std::format("Invalid variable type: `{}` is declared as {}", name,
                          type_to_string(variableType)));
    }

    handle_symbol_declaration(name, variableType, SymbolKind::VARIABLE, assignment);
}

void SemanticAnalyser::analyse_reassignment(const AST::Assignment& assignment) {
    const std::string& name = assignment.identifier_->name_;

    if (!is_symbol_declared(name)) {
        abort(std::format("Assignment to undeclared variable: `{}`", name));
    }

    const Type declaredType = get_symbol_type(name);
    const Type variableType = get_expression_type(*assignment.value_);
    if (variableType != declaredType) {
        abort(
            std::format("Type mismatch in assignment: variable `{}` is declared as {}, but "
                        "reassigned to {}",
                        name, type_to_string(declaredType), type_to_string(variableType)));
    }
}

void SemanticAnalyser::analyse_assignment(const AST::Assignment& assignment) {
    if (assignment.isDeclaration_) {
        analyse_declaration_assignment(assignment);
    } else {
        analyse_reassignment(assignment);
    }
}

void SemanticAnalyser::analyse_expression_statement(  // NOLINT(*-no-recursion)
    const AST::ExpressionStatement& exprStmt) {
    analyse_expression(*exprStmt.expression_, Type::EMPTY, "expression statement");
}

void SemanticAnalyser::analyse_if_statement(  // NOLINT(*-no-recursion)
    const AST::IfStatement& ifStmt) {
    analyse_expression(*ifStmt.condition_, Type::BOOLEAN, "condition");
    analyse_statement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyse_statement(*ifStmt.elseClause_);
    }
}

void SemanticAnalyser::analyse_while_statement(  // NOLINT(*-no-recursion)
    const AST::WhileStatement& whileStmt) {
    analyse_expression(*whileStmt.condition_, Type::BOOLEAN, "condition");
    analyse_statement(*whileStmt.body_);
}

void SemanticAnalyser::analyse_function_declaration(  // NOLINT(*-no-recursion)
    const AST::FunctionDeclaration& funcDecl) {
    analyse_statement(*funcDecl.body_);
    const std::string& name = funcDecl.identifier_->name_;
    handle_symbol_declaration(name, Type::EMPTY, SymbolKind::FUNCTION, funcDecl);
}

void SemanticAnalyser::analyse_exit(const AST::Exit& exitStmt) {
    analyse_expression(*exitStmt.exitCode_, Type::INTEGER, "exit code");
}

void SemanticAnalyser::analyse_statement(const AST::Statement& stmt) {  // NOLINT(*-no-recursion)
    switch (stmt.kind_) {
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
        case AST::NodeKind::FUNCTION_DECLARATION: {
            const auto& funcDecl = static_cast<const AST::FunctionDeclaration&>(stmt);
            analyse_function_declaration(funcDecl);
            break;
        }
        case AST::NodeKind::EXIT: {
            const auto& exitStmt = static_cast<const AST::Exit&>(stmt);
            analyse_exit(exitStmt);
            break;
        }
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = static_cast<const AST::BlockStatement&>(stmt);
            enter_scope(blockStmt);
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
