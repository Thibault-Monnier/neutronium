#pragma once

#include <optional>
#include <string>
#include <vector>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "driver/Cli.hpp"
#include "type/Type.hpp"
#include "type/TypeManager.hpp"

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

    static constexpr std::string_view ENTRY_POINT_NAME = "main";

    // Constraint helpers to centralize addConstraint calls
    void equalityConstraint(TypeID a, TypeID b, const AST::Node& node) const;
    void traitConstraint(TypeID type, Trait trait, const AST::Node& node) const;
    void subscriptConstraint(TypeID arrayType, TypeID elementType, const AST::Node& node) const;
    void storableConstraint(TypeID type, const AST::Node& node) const;

    // Register new type helpers
    TypeID registerAnyType() const { return typeManager_.createType(Type::anyFamilyType()); }
    TypeID registerIntegerType() const {
        return typeManager_.createType(Type::integerFamilyType());
    }
    TypeID registerBoolType() const { return typeManager_.createType(Type::boolType()); }
    TypeID registerVoidType() const { return typeManager_.createType(Type::voidType()); }

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

    [[nodiscard]] std::optional<const SymbolInfo*> getSymbolInfo(const std::string& name) const;
    std::optional<const SymbolInfo*> getSymbolInfoOrError(const std::string& name,
                                                          const AST::Node& node) const;

    SymbolInfo& declareSymbol(const AST::Node* declarationNode, const std::string& name,
                              SymbolKind kind, bool isMutable, TypeID typeID, bool isScoped,
                              std::vector<SymbolInfo> parameters);

    SymbolInfo& handleFunctionDeclaration(
        const AST::Node* declNode, const std::string& name, TypeID returnTypeID,
        const std::vector<std::unique_ptr<AST::VariableDefinition>>& params);
    SymbolInfo& handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                          const std::string& name, bool isMutable, TypeID typeID);

    TypeID checkFunctionCall(const AST::FunctionCall& funcCall);
    TypeID checkUnaryExpression(const AST::UnaryExpression& unaryExpr);
    TypeID checkBinaryExpression(const AST::BinaryExpression& binaryExpr);
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
    void analyseExit(const AST::ExitStatement& exitStmt);
    void analyseStatement(const AST::Statement& stmt);

    static bool verifyStatementReturns(const AST::Statement& stmt);
    void analyseExternalFunctionDeclaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyseFunctionDefinition(const AST::FunctionDefinition& funcDef);
};