#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "SymbolTable.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/diagnostics/DiagnosticsEngine.hpp"
#include "driver/Cli.hpp"
#include "frontend/source/FileID.hpp"
#include "frontend/type/Trait.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeID.hpp"
#include "frontend/type/TypeManager.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::CompilationUnit& ast, const TargetType targetType,
                              DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                              TypeManager& typeManager)
        : ast_(&ast),
          targetType_(targetType),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          typeManager_(typeManager) {
        typeManager_.getTypeSolver().prepareForConstraints();
    }

    void analyse();

   private:
    const AST::CompilationUnit* ast_;
    const TargetType targetType_;

    DiagnosticsEngine& diagnosticsEngine_;
    const FileID fileID_;
    TypeManager& typeManager_;

    std::vector<SymbolTable> scopes_;
    SymbolTable functionsTable_;

    int loopDepth_ = 0;

    std::string_view currentFunctionName_;
    TypeID currentFunctionReturnTypeID_ = 0;

    static constexpr std::string_view ENTRY_POINT_NAME = "main";

    // Constraint helpers to centralize addConstraint calls
    void equalityConstraint(TypeID a, TypeID b, const AST::Node& node) const;
    void traitConstraint(TypeID type, Trait trait, const AST::Node& node) const;
    void subscriptConstraint(TypeID arrayType, TypeID elementType, const AST::Node& node) const;
    void storableConstraint(TypeID type, const AST::Node& node) const;

    // Register new type helpers
    [[nodiscard]] TypeID registerAnyType() const {
        return typeManager_.createType(Type::anyFamilyType());
    }
    [[nodiscard]] TypeID registerIntegerType() const {
        return typeManager_.createType(Type::integerFamilyType());
    }
    [[nodiscard]] TypeID registerBoolType() const {
        return typeManager_.createType(Type::boolType());
    }
    [[nodiscard]] TypeID registerVoidType() const {
        return typeManager_.createType(Type::voidType());
    }

    void error(const std::string& errorMessage, const AST::Node& node) const;
    void fatalError(const std::string& errorMessage, const AST::Node& node) const;
    void emitErrorsAndQuit() const;

    void enterScope();
    void exitScope();

    /**
     * @class ScopeGuard
     *
     * @brief A helper class for managing the scope lifecycle within the SemanticAnalyser.
     *
     * ScopeGuard is responsible for ensuring that an entered scope in the associated
     * SemanticAnalyser is properly exited when the ScopeGuard goes out of scope.
     * It automatically calls `enterScope()` on construction and `exitScope()` on destruction
     * for the provided SemanticAnalyser instance.
     *
     * This class prevents copying and moving to ensure the integrity of scope management.
     */
    class ScopeGuard {
       public:
        explicit ScopeGuard(SemanticAnalyser& analyser) : analyser_(analyser) {
            analyser_.enterScope();
        }
        ~ScopeGuard() { analyser_.exitScope(); }
        ScopeGuard(const ScopeGuard&) = delete;
        ScopeGuard& operator=(const ScopeGuard&) = delete;
        ScopeGuard(ScopeGuard&&) = delete;
        ScopeGuard& operator=(ScopeGuard&&) = delete;

       private:
        SemanticAnalyser& analyser_;
    };

    void handleUndeclaredSymbolError(std::string_view name, const AST::Node& node,
                                     SymbolKind kind) const;

    [[nodiscard]] std::optional<const SymbolInfo*> getFunctionSymbolInfo(
        std::string_view name) const;
    [[nodiscard]] std::optional<const SymbolInfo*> getVariableSymbolInfo(
        std::string_view name) const;
    [[nodiscard]] std::optional<const SymbolInfo*> getSymbolInfo(std::string_view name) const;
    [[nodiscard]] std::optional<const SymbolInfo*> getFunctionSymbolInfoOrError(
        std::string_view name, const AST::Node& node) const;
    [[nodiscard]] std::optional<const SymbolInfo*> getVariableSymbolInfoOrError(
        std::string_view name, const AST::Node& node) const;

    void ensureSymbolUndeclaredOrError(const AST::Node* declarationNode,
                                       std::string_view name) const;

    SymbolInfo& handleFunctionDeclaration(const AST::Node* declNode, std::string_view name,
                                          std::span<AST::VariableDefinition*> params);
    SymbolInfo& handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                          std::string_view name);

    TypeID checkFunctionCall(const AST::FunctionCall& funcCall);
    TypeID checkUnaryExpression(const AST::UnaryExpression& unaryExpr);
    TypeID checkBinaryExpression(const AST::BinaryExpression& binaryExpr);
    void emptyArrayLiteralError(const AST::Expression& arrayLit) const;
    TypeID checkExpression(const AST::Expression& expr);
    void checkExpression(const AST::Expression& expr, TypeID expected);

    void analyseVariableDefinition(const AST::VariableDefinition& definition);
    bool verifyIsAssignable(const AST::Expression& expr);
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
    void analyseExternalFunctionDeclaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyseFunctionDefinition(const AST::FunctionDefinition& funcDef);
};