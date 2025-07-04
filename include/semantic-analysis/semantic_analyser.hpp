#pragma once

#include <string>
#include <vector>

#include "cli.hpp"
#include "parsing/ast.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "semantic-analysis/type.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast, const TargetType targetType)
        : ast_(&ast), targetType_(targetType) {}

    void analyse();

   private:
    const AST::Program* ast_;
    const TargetType targetType_;

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
                               bool isScoped, std::vector<SymbolInfo> parameters);

    SymbolInfo& handle_constant_definition(const std::string& name, Type type);
    SymbolInfo& handle_function_declaration(
        const std::string& name, Type returnType,
        const std::vector<std::unique_ptr<AST::VariableDefinition>>& params);
    SymbolInfo& handle_variable_declaration(const std::string& name, bool isMutable, Type type);

    // ── Expression utilities ───────────────────────────────────────────────────
    Type get_function_call_type(const AST::FunctionCall& funcCall);
    Type get_unary_expression_type(const AST::UnaryExpression& unaryExpr);
    Type get_binary_expression_type(const AST::BinaryExpression& binaryExpr);
    Type get_expression_type(const AST::Expression& expr);

    void analyse_expression(const AST::Expression& expr, Type expected,
                            const std::string& location);

    // ── Statement analysis ─────────────────────────────────────────────────────
    void analyse_variable_definition(const AST::VariableDefinition& declaration);
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
    void analyse_external_function_declaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyse_function_definition(const AST::FunctionDefinition& funcDef);

    // ── Constant analysis ───────────────────────────────────────────
    void analyse_constant_definition(const AST::ConstantDefinition& declaration);
};