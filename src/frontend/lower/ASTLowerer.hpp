#pragma once

#include "frontend/ast/AST.hpp"
#include "frontend/type/TypeManager.hpp"
#include "ir/build/Builder.hpp"
#include "ir/core/IR.hpp"

/// Responsible for creating an IR based on the AST.
class ASTLowerer {
    const AST::CompilationUnit& ast_;
    const TypeManager& typeManager_;

    IR::Module ir_;
    IR::Builder builder_;

    IR::BasicBlock* currentBreakBlock_ = nullptr;
    IR::BasicBlock* currentContinueBlock_ = nullptr;

   public:
    explicit ASTLowerer(const AST::CompilationUnit& ast, const TypeManager& typeManager)
        : ast_(ast), typeManager_(typeManager), builder_() {}

    [[nodiscard]] IR::Module&& lower();

   private:
    [[nodiscard]] IR::Type convertPrimitiveType(const Type& type) const;
    const IR::Type& convertType(TypeID typeID);

    void declareFunction(std::string_view name, std::span<AST::VariableDefinition*> parameters,
                         TypeID returnTypeID);

    void lowerExternalFunction(const AST::ExternalFunctionDeclaration& funcDecl) {
        declareFunction(funcDecl.identifier_->name_, funcDecl.parameters_, funcDecl.returnTypeID_);
    }

    void lowerFunction(const AST::FunctionDefinition& funcDef) {
        declareFunction(funcDef.identifier_->name_, funcDef.parameters_, funcDef.returnTypeID_);

        for (const auto* stmt : funcDef.body_->body_) lowerStatement(*stmt);
    }

    void lowerStatement(const AST::Statement& stmt);

    void lowerVariableDefinition(const AST::VariableDefinition& varDef);
    void lowerAssignment(const AST::Assignment& assignment);
    void lowerExpressionStatement(const AST::ExpressionStatement& exprStmt);
    void lowerIfStatement(const AST::IfStatement& ifStmt);
    void lowerWhileStatement(const AST::WhileStatement& whileStmt);
    void lowerBreakStatement();
    void lowerContinueStatement();
    void lowerReturnStatement(const AST::ReturnStatement& returnStmt);
    void lowerExitStatement(const AST::ExitStatement& exitStmt);

    IR::Value& lowerExpression(const AST::Expression& expr);

    IR::Value& lowerNumberLiteral(const AST::NumberLiteral& numberLit);
    IR::Value& lowerBooleanLiteral(const AST::BooleanLiteral& boolLit);
    IR::Value& lowerIdentifier(const AST::Identifier& identifier);
    IR::Value& lowerFunctionCall(const AST::FunctionCall& funcCall);
    IR::Value& lowerArrayAccess(const AST::ArrayAccess& arrayAccess);
    IR::Value& lowerArrayLiteral(const AST::ArrayLiteral& arrayLit);
    IR::Value& lowerRepeatArrayLiteral(const AST::RepeatArrayLiteral& repeatArrayLit);
    IR::Value& lowerUnaryExpression(const AST::UnaryExpression& unaryExpr);

    IR::Value& lowerBinaryExpression(const AST::Expression& left, const AST::Expression& right,
                                     AST::Operator op);
    IR::Value& lowerBinaryExpression(const AST::BinaryExpression& binaryExpr) {
        return lowerBinaryExpression(*binaryExpr.left_, *binaryExpr.right_, binaryExpr.operator_);
    }

    IR::Value& lowerLogicalAndExpression(const AST::Expression& left, const AST::Expression& right);
    IR::Value& lowerLogicalOrExpression(const AST::Expression& left, const AST::Expression& right);
};
