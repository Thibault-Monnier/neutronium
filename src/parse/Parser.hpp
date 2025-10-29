#pragma once

#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "ast/AST.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Token.hpp"
#include "type/TypeManager.hpp"

struct ParsedFunctionSignature {
    std::unique_ptr<AST::Identifier> identifier_;
    std::vector<std::unique_ptr<AST::VariableDefinition>> parameters_;
    TypeID returnTypeID_;
};

class Parser {
   public:
    explicit Parser(std::vector<Token> tokens, DiagnosticsEngine& diagnosticsEngine,
                    TypeManager& typeManager)
        : diagnosticsEngine_(diagnosticsEngine),
          typeManager_(typeManager),
          tokens_(std::move(tokens)) {}

    [[nodiscard]] std::unique_ptr<AST::Program> parse();

   private:
    DiagnosticsEngine& diagnosticsEngine_;

    TypeManager& typeManager_;

    std::vector<Token> tokens_;
    size_t currentIndex_ = 0;

    [[noreturn]] void abort(const std::string& errorMessage) const;

    [[nodiscard]] const Token& peek(int amount = 0) const;
    const Token& expect(TokenKind expected);

    /** Gets an instance of Type::anyFamilyType() and registers it in the TypeManager.
     * @return The TypeID of the newly created Type.
     */
    [[nodiscard]] TypeID generateAnyType() const {
        return typeManager_.createType(Type::anyFamilyType());
    }

    Type parseTypeSpecifier();
    std::unique_ptr<AST::NumberLiteral> parseNumberLiteral();
    std::unique_ptr<AST::ArrayLiteral> parseArrayLiteral();

    std::unique_ptr<AST::Identifier> parseIdentifier();
    std::unique_ptr<AST::FunctionCall> parseFunctionCall();
    std::unique_ptr<AST::ArrayAccess> parseArrayAccess(std::unique_ptr<AST::Expression>& base);
    std::unique_ptr<AST::Expression> parsePrimaryExpression();
    std::unique_ptr<AST::Expression> parsePostfixExpression();
    std::unique_ptr<AST::Expression> parseUnaryExpression();
    std::unique_ptr<AST::Expression> parseBinaryExpression(
        const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
        const std::set<AST::Operator>& allowedOps, bool allowMultiple);
    std::unique_ptr<AST::Expression> parseMultiplicativeExpression();
    std::unique_ptr<AST::Expression> parseAdditiveExpression();
    std::unique_ptr<AST::Expression> parseComparisonExpression();
    std::unique_ptr<AST::Expression> parseExpression();

    std::unique_ptr<AST::ExpressionStatement> parseExpressionStatement();
    std::unique_ptr<AST::VariableDefinition> parseVariableDefinition();
    std::unique_ptr<AST::Assignment> parseAssignment();
    std::unique_ptr<AST::BlockStatement> parseElseClause();
    std::unique_ptr<AST::IfStatement> parseIfStatement();
    std::unique_ptr<AST::WhileStatement> parseWhileStatement();
    std::unique_ptr<AST::VariableDefinition> parseFunctionParameter();
    std::unique_ptr<AST::BreakStatement> parseBreakStatement();
    std::unique_ptr<AST::ContinueStatement> parseContinueStatement();
    std::unique_ptr<AST::ReturnStatement> parseReturnStatement();
    std::unique_ptr<AST::ExitStatement> parseExitStatement();
    std::unique_ptr<AST::BlockStatement> parseBlockStatement();
    std::unique_ptr<AST::Statement> parseStatement();
    ParsedFunctionSignature parseFunctionSignature();
    std::unique_ptr<AST::ExternalFunctionDeclaration> parseExternalFunctionDeclaration();

    std::unique_ptr<AST::FunctionDefinition> parseFunctionDefinition();
    std::unique_ptr<AST::Program> parseProgram();
};
