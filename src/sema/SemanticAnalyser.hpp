#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "driver/Cli.hpp"
#include "source/FileID.hpp"
#include "type/Trait.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::Program& ast, const TargetType targetType,
                              DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                              TypeManager& typeManager)
        : ast_(&ast),
          targetType_(targetType),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          typeManager_(typeManager) {}

    void analyse();

   private:
    const AST::Program* ast_;
    const TargetType targetType_;

    DiagnosticsEngine& diagnosticsEngine_;
    const FileID fileID_;
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

    [[nodiscard]] std::optional<const SymbolInfo*> getSymbolInfo(std::string_view name) const;
    [[nodiscard]] std::optional<const SymbolInfo*> getSymbolInfoOrError(
        std::string_view name, const AST::Node& node) const;

    SymbolInfo& declareSymbol(const AST::Node* declarationNode, std::string_view name,
                              SymbolKind kind, bool isMutable, TypeID typeID, bool isScoped,
                              std::vector<SymbolInfo> parameters);

    SymbolInfo& handleFunctionDeclaration(const AST::Node* declNode, std::string_view name,
                                          TypeID returnTypeID,
                                          const std::vector<AST::VariableDefinition*>& params);
    SymbolInfo& handleVariableDeclaration(const AST::VariableDefinition* declNode,
                                          std::string_view name, bool isMutable, TypeID typeID);

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
    void analyseReturnStatement(const AST::ReturnStatement& returnStmt);
    void analyseExit(const AST::ExitStatement& exitStmt);
    void analyseStatement(const AST::Statement& stmt);

    static bool verifyStatementReturns(const AST::Statement& stmt);
    void analyseExternalFunctionDeclaration(const AST::ExternalFunctionDeclaration& funcDecl);
    void analyseFunctionDefinition(const AST::FunctionDefinition& funcDef);
};