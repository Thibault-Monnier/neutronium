#pragma once

#include "frontend/ast/AST.hpp"
#include "frontend/sema/SymbolTable.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeID.hpp"
#include "frontend/type/TypeManager.hpp"

namespace Sema {

class ConstraintGenerator {
   public:
    explicit ConstraintGenerator(const AST::CompilationUnit& ast,
                                 DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                                 TypeManager& typeManager, TypeSolver& typeSolver,
                                 SymbolTableMap& functionsTable)
        : ast_(&ast),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          typeManager_(typeManager),
          typeSolver_(typeSolver),
          functionsTable_(functionsTable) {
        assert(functionsTable_.empty());
    }

    /// Traverses the AST and generates all type constraints.
    void traverse();

   private:
    const AST::CompilationUnit* ast_;

    DiagnosticsEngine& diagnosticsEngine_;
    const FileID fileID_;
    TypeManager& typeManager_;
    TypeSolver& typeSolver_;
    SymbolTableMap& functionsTable_;

    SymbolTableList scopedVariables_;
    std::vector<size_t> scopeBoundaries_;

    TypeID currentFunctionReturnTypeID_;

    // Constraint helpers to centralize addConstraint calls
    void equalityConstraint(TypeID a, TypeID b, const AST::Node& node) const;
    void traitConstraint(TypeID type, Trait trait, const AST::Node& node) const;
    void subscriptConstraint(TypeID arrayType, TypeID elementType, const AST::Node& node) const;
    void storableConstraint(TypeID type, const AST::Node& node) const;

    // Register new type helpers
    [[nodiscard]] TypeID registerTypeVariable() const { return typeManager_.createTypeVariable(); }
    [[nodiscard]] TypeID registerIntegerType() const {
        return typeManager_.createType(Type::integerFamilyType());
    }
    [[nodiscard]] TypeID registerBoolType() const {
        return typeManager_.createType(Type::boolType());
    }
    [[nodiscard]] TypeID registerCharType() const {
        return typeManager_.createType(Type::charType());
    }
    [[nodiscard]] TypeID registerVoidType() const {
        return typeManager_.createType(Type::voidType());
    }

    void emitErrorsAndQuit() const;
    void error(const std::string& errorMessage, const AST::Node& node) const;
    void fatalError(const std::string& errorMessage, const AST::Node& node) const;

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
        explicit ScopeGuard(ConstraintGenerator& cg) : cg_(cg) { cg_.enterScope(); }
        ~ScopeGuard() { cg_.exitScope(); }

        ScopeGuard(const ScopeGuard&) = delete;
        ScopeGuard& operator=(const ScopeGuard&) = delete;
        ScopeGuard(ScopeGuard&&) = delete;
        ScopeGuard& operator=(ScopeGuard&&) = delete;

       private:
        ConstraintGenerator& cg_;
    };

    void handleUndeclaredSymbolError(std::string_view name, const AST::Node& node,
                                     SymbolKind kind) const;

    [[nodiscard]] std::optional<SymbolInfo> getFunctionSymbolInfo(std::string_view name) const;
    [[nodiscard]] std::optional<SymbolInfo> getVariableSymbolInfo(std::string_view name) const;
    [[nodiscard]] std::optional<SymbolInfo> getSymbolInfo(std::string_view name) const;
    [[nodiscard]] std::optional<SymbolInfo> getFunctionSymbolInfoOrError(
        std::string_view name, const AST::Node& node) const;
    [[nodiscard]] std::optional<SymbolInfo> getVariableSymbolInfoOrError(
        std::string_view name, const AST::Node& node) const;

    void ensureSymbolUndeclaredOrError(const AST::Node* declarationNode,
                                       std::string_view name) const;

    void handleFunctionDeclaration(const AST::Node* declNode, std::string_view name,
                                   std::span<AST::VariableDefinition*> params);
    void handleVariableDeclaration(const AST::VariableDefinition* declNode, std::string_view name);

   private:
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
    void analyseReturnStatement(const AST::ReturnStatement& returnStmt);
    void analyseExit(const AST::ExitStatement& exitStmt);
    void analyseStatement(const AST::Statement& stmt);

    void analyseExternalFunctionDeclaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyseFunctionDefinition(const AST::FunctionDefinition& funcDef);
};

}  // namespace Sema
