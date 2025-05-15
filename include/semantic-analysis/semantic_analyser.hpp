#pragma once

#include <string>
#include <vector>

#include "parsing/ast.hpp"
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
    std::vector<Scope> scopes_;

    int loopDepth_ = 0;

    [[noreturn]] static void abort(const std::string& errorMessage, const std::string& hintMessage = "");

    void enter_scope();
    void exit_scope();

    bool is_symbol_declared(const std::string& name) const;
    Type get_symbol_type(const std::string& name) const;
    SymbolKind get_symbol_kind(const std::string& name) const;
    void handle_symbol_declaration(const std::string& name, bool isMutable, Type type,
                                   SymbolKind kind, const AST::Node& declarationNode);
    Type get_function_call_type(const AST::FunctionCall& funcCall);

    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr);
    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr);
    Type get_expression_type(const AST::Expression& expr);
    void analyse_expression(const AST::Expression& expr, Type expected,
                            const std::string& location);

    void analyse_variable_declaration(const AST::VariableDeclaration& declaration);
    void analyse_variable_assignment(const AST::VariableAssignment& assignment);
    void analyse_expression_statement(const AST::ExpressionStatement& exprStmt);

    void analyse_if_statement(const AST::IfStatement& ifStmt);
    void analyse_while_statement(const AST::WhileStatement& whileStmt);
    void analyse_function_declaration(const AST::FunctionDeclaration& funcDecl);
    void analyse_break_statement() const;
    void analyse_continue_statement() const;
    void analyse_exit(const AST::Exit& exitStmt);

    void analyse_statement(const AST::Statement& stmt);
};