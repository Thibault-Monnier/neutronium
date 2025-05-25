#pragma once

#include <string>
#include <vector>

#include "parsing/ast.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "semantic-analysis/type.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast) : ast_(&ast) {}

    void analyse();

   private:
    const AST::Program* ast_;

    std::vector<SymbolTable> scopes_;
    SymbolTable functionsTable_;

    int loopDepth_ = 0;

    std::string currentFunctionName_;
    Type currentFunctionReturnType_ = RawType::VOID;

    [[noreturn]] static void abort(const std::string& errorMessage);

    void enter_scope();
    void exit_scope();

    // ── Symbol utilities ────────────────────────────────────────────────────────
    [[nodiscard]] std::optional<const SymbolInfo*> get_symbol_info(const std::string& name) const;

    // ── Symbol declaration helpers ──────────────────────────────────────────────
    SymbolInfo& declare_symbol(const std::string& name, SymbolKind kind, bool isMutable, Type type,
                               bool isScoped, const AST::Node& declarationNode,
                               std::vector<SymbolInfo> parameters);

    SymbolInfo& handle_constant_declaration(const std::string& name, Type type,
                                            const AST::Node& declarationNode);
    SymbolInfo& handle_function_declaration(const std::string& name, Type returnType,
                                            const std::vector<SymbolInfo>& parameterSymbols,
                                            const AST::Node& declarationNode);
    SymbolInfo& handle_variable_declaration(const std::string& name, bool isMutable, Type type,
                                            const AST::Node& declarationNode);

    // ── Expression utilities ───────────────────────────────────────────────────
    Type get_function_call_type(const AST::FunctionCall& funcCall);
    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr);
    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr);
    Type get_expression_type(const AST::Expression& expr);

    void analyse_expression(const AST::Expression& expr, Type expected,
                            const std::string& location);

    // ── Statement analysis ─────────────────────────────────────────────────────
    void analyse_variable_declaration(const AST::VariableDeclaration& declaration);
    void analyse_variable_assignment(const AST::VariableAssignment& assignment);
    void analyse_expression_statement(const AST::ExpressionStatement& exprStmt);
    void analyse_if_statement(const AST::IfStatement& ifStmt);
    void analyse_while_statement(const AST::WhileStatement& whileStmt);
    void analyse_break_statement() const;
    void analyse_continue_statement() const;
    void analyse_exit(const AST::ExitStatement& exitStmt);
    void analyse_statement(const AST::Statement& stmt);

    // ── Function analysis ──────────────────────────────────────────────────────
    bool verify_statement_returns(const AST::Statement& stmt);
    void analyse_function_declaration(const AST::FunctionDeclaration& funcDecl);

    // ── Constant analysis ───────────────────────────────────────────
    void analyse_constant_declaration(const AST::ConstantDeclaration& declaration);
};