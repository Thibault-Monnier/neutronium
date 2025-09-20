#pragma once

#include <optional>
#include <string>
#include <vector>

#include "cli.hpp"
#include "diagnostics_engine.hpp"
#include "parsing/ast.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "types/Type.hpp"
#include "types/TypeManager.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast, const TargetType targetType,
                              DiagnosticsEngine& diagnosticsEngine, TypeManager& typeManager)
        : ast_(&ast),
          targetType_(targetType),
          diagnosticsEngine_(diagnosticsEngine),
          typeManager_(typeManager) {}

    void analyse();

   private:
    const AST::Program* ast_;
    const TargetType targetType_;

    DiagnosticsEngine& diagnosticsEngine_;
    TypeManager& typeManager_;

    std::vector<SymbolTable> scopes_;
    SymbolTable functionsTable_;

    int loopDepth_ = 0;

    std::string currentFunctionName_;
    TypeID currentFunctionReturnTypeID_ = 0;

    [[noreturn]] void abort(const std::string& errorMessage, const AST::Node& node) const;

    void enter_scope();
    void exit_scope();

    // ── Symbol utilities ────────────────────────────────────────────────────────
    [[nodiscard]] std::optional<const SymbolInfo*> get_symbol_info(const std::string& name) const;

    // ── Symbol declaration helpers ──────────────────────────────────────────────
    SymbolInfo& declare_symbol(const AST::Node* declarationNode, const std::string& name,
                               SymbolKind kind, bool isMutable, TypeID typeID, bool isScoped,
                               std::vector<SymbolInfo> parameters);

    SymbolInfo& handle_function_declaration(
        const AST::Node* declNode, const std::string& name, TypeID returnTypeID,
        const std::vector<std::unique_ptr<AST::VariableDefinition>>& params);
    SymbolInfo& handle_variable_declaration(const AST::VariableDefinition* declNode,
                                            const std::string& name, bool isMutable, TypeID typeID);

    // ── Expression utilities ───────────────────────────────────────────────────
    TypeID get_function_call_type(const AST::FunctionCall& funcCall);
    TypeID get_unary_expression_type(const AST::UnaryExpression& unaryExpr);
    TypeID get_binary_expression_type(const AST::BinaryExpression& binaryExpr);
    TypeID get_expression_type(const AST::Expression& expr);

    void analyse_expression(const AST::Expression& expr, TypeID expected);

    // ── Statement analysis ─────────────────────────────────────────────────────
    void analyse_variable_definition(const AST::VariableDefinition& definition);
    void analyse_assignment(const AST::Assignment& assignment);
    void analyse_expression_statement(const AST::ExpressionStatement& exprStmt);
    void analyse_if_statement(const AST::IfStatement& ifStmt);
    void analyse_while_statement(const AST::WhileStatement& whileStmt);
    void analyse_break_statement(const AST::BreakStatement& breakStmt) const;
    void analyse_continue_statement(const AST::ContinueStatement& continueStmt) const;
    void analyse_exit(const AST::ExitStatement& exitStmt);
    void analyse_statement(const AST::Statement& stmt);

    // ── Function analysis ──────────────────────────────────────────────────────
    static bool verify_statement_returns(const AST::Statement& stmt);
    void analyse_external_function_declaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyse_function_definition(const AST::FunctionDefinition& funcDef);
};