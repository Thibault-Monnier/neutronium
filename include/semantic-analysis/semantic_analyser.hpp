#pragma once

#include <string>
#include <vector>

#include "parsing/AST.hpp"
#include "semantic-analysis/scope.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "semantic-analysis/type.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast) : ast_(&ast) {}

    SymbolTable analyse();

   private:
    const AST::Program* ast_;

    SymbolTable symbolTable_;

    int currentStackOffset_ = 0;
    std::vector<Scope> scopeVariablesStackOffset_;

    [[noreturn]] void abort(const std::string& errorMessage, const std::string& hintMessage = "");

    void enter_scope(const AST::BlockStatement& blockStmt);
    void exit_scope();

    bool is_variable_declared_in_scope(const std::string& name);
    Type get_scope_variable_type(const std::string& name);

    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr);
    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr);
    Type get_expression_type(const AST::Expression& expr);
    void analyse_expression(const AST::Expression& expr, const Type expected,
                            const std::string& location);

    void analyse_declaration_assignment(const AST::Assignment& assignment);
    void analyse_reassignment(const AST::Assignment& assignment);
    void analyse_assignment(const AST::Assignment& assignment);

    void analyse_if_statement(const AST::IfStatement& ifStmt);
    void analyse_while_statement(const AST::WhileStatement& whileStmt);
    void analyse_exit(const AST::Exit& exitStmt);

    void analyse_statement(const AST::Statement& stmt);
};