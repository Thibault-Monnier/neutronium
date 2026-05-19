#pragma once

#include <ranges>

#include "frontend/ast/AST.hpp"
#include "frontend/type/TypeManager.hpp"
#include "ir/build/Builder.hpp"
#include "ir/core/IR.hpp"

/// Responsible for creating an IR based on the AST.
class ASTLowerer {
    const AST::CompilationUnit& ast_;
    const TypeManager& typeManager_;

    IR::Builder builder_;

    IR::BasicBlock* currentBreakBlock_ = nullptr;
    IR::BasicBlock* currentContinueBlock_ = nullptr;

    std::vector<std::unordered_map<std::string_view, IR::Value*>> scopedSymbolAdresses_;

   public:
    explicit ASTLowerer(const AST::CompilationUnit& ast, const TypeManager& typeManager,
                        IR::Module& ir, neutro::PolymorphicArenaAllocator& arena)
        : ast_(ast), typeManager_(typeManager), builder_(ir, arena) {}

    void lower();

   private:
    [[nodiscard]] const IR::Type& convertPrimitiveType(const Type& type) const;
    const IR::Type& convertType(TypeID typeID);

    /// RAII helper to manage entering and exiting scopes. Enters a new scope on construction and
    /// exits it on destruction.
    class ScopeGuard {
        ASTLowerer& lowerer_;

       public:
        explicit ScopeGuard(ASTLowerer& lowerer) : lowerer_(lowerer) { lowerer.enterScope(); }
        ~ScopeGuard() { lowerer_.exitScope(); }
    };

    void enterScope() { scopedSymbolAdresses_.emplace_back(); }
    void exitScope() { scopedSymbolAdresses_.pop_back(); }

    void declareSymbol(std::string_view name, IR::Value* value);
    [[nodiscard]] IR::Value& lookupSymbolAddress(std::string_view name) const;

    void declareFunction(std::string_view name, std::span<AST::VariableDefinition*> parameters,
                         TypeID returnTypeID, bool isExported, bool isExternal);

    void lowerExternalFunction(const AST::ExternalFunctionDeclaration& funcDecl) {
        declareFunction(funcDecl.identifier_->name_, funcDecl.parameters_, funcDecl.returnTypeID_,
                        false, true);
    }

    void lowerFunction(const AST::FunctionDefinition& funcDef);

    void lowerStatement(const AST::Statement& stmt);

    void lowerAssignment(const AST::Assignment& assignment);
    void lowerVariableDefinition(const AST::VariableDefinition& varDef);

    void lowerExpressionStatement(const AST::ExpressionStatement& exprStmt);
    void lowerIfStatement(const AST::IfStatement& ifStmt);
    void lowerWhileStatement(const AST::WhileStatement& whileStmt);
    void lowerBreakStatement();
    void lowerContinueStatement();
    void lowerReturnStatement(const AST::ReturnStatement& returnStmt);
    void lowerExitStatement(const AST::ExitStatement& exitStmt);

    IR::Value& lowerValueExpression(const AST::Expression& expr, std::optional<IR::Value*> place);
    IR::Value& lowerValueExpression(const AST::Expression& expr) {
        return lowerValueExpression(expr, std::nullopt);
    }
    IR::Value& lowerPlaceExpression(const AST::Expression& expr);

    IR::Value& lowerNumberLiteral(const AST::NumberLiteral& numberLit);
    IR::Value& lowerBooleanLiteral(const AST::BooleanLiteral& boolLit);
    [[nodiscard]] IR::Value& lowerIdentifierAddress(const AST::Identifier& identifier) const;
    IR::Value& lowerFunctionCall(const AST::FunctionCall& funcCall);
    IR::Value& lowerArrayAccessAddress(const AST::ArrayAccess& arrayAccess);
    IR::Value& lowerArrayLiteral(const AST::ArrayLiteral& arrayLit,
                                 std::optional<IR::Value*> place);
    IR::Value& lowerRepeatArrayLiteral(const AST::RepeatArrayLiteral& repeatArrayLit,
                                       std::optional<IR::Value*> place);
    IR::Value& lowerUnaryExpression(const AST::UnaryExpression& unaryExpr);

    IR::Value& lowerBinaryExpression(const AST::Expression& left, const AST::Expression& right,
                                     AST::Operator op);
    IR::Value& lowerBinaryExpression(const AST::BinaryExpression& binaryExpr) {
        return lowerBinaryExpression(*binaryExpr.left_, *binaryExpr.right_, binaryExpr.operator_);
    }

    IR::Value& lowerLogicalAndExpression(const AST::Expression& left, const AST::Expression& right);
    IR::Value& lowerLogicalOrExpression(const AST::Expression& left, const AST::Expression& right);
};
