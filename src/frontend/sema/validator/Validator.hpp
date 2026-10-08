#pragma once

#include "driver/Cli.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/diagnostics/DiagnosticsEngine.hpp"
#include "frontend/sema/SymbolTable.hpp"
#include "frontend/type/TypeManager.hpp"

namespace Sema {

class Validator {
   public:
    explicit Validator(const AST::CompilationUnit& ast, const TargetType targetType,
                       DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                       TypeManager& typeManager, SymbolTableMap& functionsTable)
        : ast_(&ast),
          targetType_(targetType),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          typeManager_(typeManager),
          functionsTable_(functionsTable) {}

    /// Traverses the AST with types resolved, and performs semantic validation.
    void traverse();

   private:
    const AST::CompilationUnit* ast_;
    const TargetType targetType_;

    DiagnosticsEngine& diagnosticsEngine_;
    const FileID fileID_;
    TypeManager& typeManager_;

    SymbolTableMap& functionsTable_;

    uint32_t loopDepth_ = 0;

    static constexpr std::string_view ENTRY_POINT_NAME = "main";

    void error(const std::string& errorMessage, const AST::Node& node) const;

    void checkExpression(const AST::Expression& expr);

    void analyseVariableDefinition(const AST::VariableDefinition& definition);
    void analyseAssignment(const AST::Assignment& assignment);
    void analyseExpressionStatement(const AST::ExpressionStatement& exprStmt);
    void analyseIfStatement(const AST::IfStatement& ifStmt);
    void analyseWhileStatement(const AST::WhileStatement& whileStmt);
    void analyseBreakStatement(const AST::BreakStatement& breakStmt) const;
    void analyseContinueStatement(const AST::ContinueStatement& continueStmt) const;
    void analyseReturnStatement(const AST::ReturnStatement& returnStmt);
    void analyseExit(const AST::ExitStatement& exitStmt);
    void analyseStatement(const AST::Statement& stmt);

    static bool verifyStatementReturns(const AST::Statement& stmt);
    void analyseFunctionDefinition(const AST::FunctionDefinition& funcDef);
};

}  // namespace Sema
