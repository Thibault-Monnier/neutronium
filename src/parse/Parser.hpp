#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "ast/AST.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Lexer.hpp"
#include "lex/Token.hpp"
#include "type/TypeManager.hpp"

struct ParsedFunctionSignature {
    std::unique_ptr<AST::Identifier> identifier_;
    std::vector<std::unique_ptr<AST::VariableDefinition>> parameters_;
    TypeID returnTypeID_;
};

class Parser {
   public:
    explicit Parser(DiagnosticsEngine& diagnosticsEngine, const std::string_view sourceCode,
                    TypeManager& typeManager)
        : lexer_(sourceCode, diagnosticsEngine),
          diagnosticsEngine_(diagnosticsEngine),
          sourceCode_(sourceCode),
          typeManager_(typeManager) {
        token_ = lexer_.lex();
    }

    [[nodiscard]] std::unique_ptr<AST::Program> parse();

   private:
    Lexer lexer_;

    DiagnosticsEngine& diagnosticsEngine_;
    std::string_view sourceCode_;

    TypeManager& typeManager_;

    // --------------
    // Error handling
    // --------------

    /** Emits an error with the given message at the location of the given token.
     * @param errorMessage The error message to emit.
     * @param token The token where the error occurred.
     */
    void emitError(const std::string& errorMessage, Token token) const;
    /** Emits an error and returns nullptr of the template type.
     * @param errorMessage The error message to emit.
     * @param token The token where the error occurred.
     * @tparam T The type of the nullptr to return.
     * @return nullptr of type T.
     */
    template <class T>
    [[nodiscard]] std::unique_ptr<T> emitError(const std::string& errorMessage,
                                               const Token token) const {
        emitError(errorMessage, token);
        return nullptr;
    }
    /** Emits an error at the current token and returns nullptr of the template type.
     * @param errorMessage The error message to emit.
     * @tparam T The type of the nullptr to return.
     * @return nullptr of type T.
     */
    template <class T>
    [[nodiscard]] std::unique_ptr<T> emitError(const std::string& errorMessage) const {
        return emitError<T>(errorMessage, peek());
    }

    void expectError(TokenKind expected) const;
    [[nodiscard]] std::unique_ptr<Type> invalidTypeSpecifierError() const;
    [[nodiscard]] std::unique_ptr<AST::Expression> invalidPrimaryExpressionError() const;

    // ---------------
    // Parsing helpers
    // ---------------

    Token token_ = Token::dummy();

    [[nodiscard]] Token peek() const { return token_; }
    void advance() { token_ = lexer_.lex(); }
    inline bool advanceIf(TokenKind expected);
    inline bool expect(TokenKind expected, Token& outToken);

    /** Gets an instance of Type::anyFamilyType() and registers it in the TypeManager.
     * @return The TypeID of the newly created Type.
     */
    [[nodiscard]] TypeID generateAnyType() const {
        return typeManager_.createType(Type::anyFamilyType());
    }

    template <class T>
    std::optional<std::vector<std::unique_ptr<T>>> parseCommaSeparatedList(
        TokenKind endDelimiter, const std::function<std::unique_ptr<T>()>& parseElement);
    std::optional<std::vector<std::unique_ptr<AST::Expression>>> parseExpressionList(
        TokenKind endDelimiter);

    static std::optional<Type> tryParsePrimitiveType(TokenKind tokenKind);
    std::unique_ptr<Type> parseTypeSpecifier();
    /** Parses a type annotation if the next token matches `typeAnnotationIndicator`, and returns it
     * or std::nullopt if parsing failed.
     * If the next token does not match `typeAnnotationIndicator`, returns `defaultType`.
     */
    std::optional<Type> maybeParseTypeAnnotation(TokenKind typeAnnotationIndicator,
                                                 Type defaultType);

    std::unique_ptr<AST::NumberLiteral> parseNumberLiteral();
    std::unique_ptr<AST::ArrayLiteral> parseArrayLiteral();

    std::unique_ptr<AST::Identifier> parseIdentifier();
    std::unique_ptr<AST::Expression> parseIdentifierOrFunctionCall();
    std::unique_ptr<AST::ArrayAccess> parseArrayAccess(std::unique_ptr<AST::Expression> base);
    std::unique_ptr<AST::Expression> parsePrimaryExpression();
    std::unique_ptr<AST::Expression> parsePostfixExpression();
    std::unique_ptr<AST::Expression> parseUnaryExpression();
    std::unique_ptr<AST::Expression> parseBinaryExpression(
        const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
        std::initializer_list<AST::Operator> allowedOps, bool allowMultiple);
    std::unique_ptr<AST::Expression> parseMultiplicativeExpression();
    std::unique_ptr<AST::Expression> parseAdditiveExpression();
    std::unique_ptr<AST::Expression> parseComparisonExpression();
    std::unique_ptr<AST::Expression> parseExpression();

    std::unique_ptr<AST::VariableDefinition> parseVariableDefinition();
    std::unique_ptr<AST::Statement> parseAssignmentOrExpressionStatement();

    std::unique_ptr<AST::BlockStatement> parseElseClause();
    std::unique_ptr<AST::IfStatement> parseIfOrElif(TokenKind kind);
    std::unique_ptr<AST::IfStatement> parseIfStatement();
    std::unique_ptr<AST::WhileStatement> parseWhileStatement();

    std::unique_ptr<AST::VariableDefinition> parseFunctionParameter();
    std::unique_ptr<AST::BreakStatement> parseBreakStatement();
    std::unique_ptr<AST::ContinueStatement> parseContinueStatement();
    std::unique_ptr<AST::ReturnStatement> parseReturnStatement();
    std::unique_ptr<AST::ExitStatement> parseExitStatement();
    std::unique_ptr<AST::BlockStatement> parseBlockStatement();

    std::unique_ptr<AST::Statement> parseStatement();

    std::unique_ptr<ParsedFunctionSignature> parseFunctionSignature();
    std::unique_ptr<AST::ExternalFunctionDeclaration> parseExternalFunctionDeclaration();

    std::unique_ptr<AST::FunctionDefinition> parseFunctionDefinition();
    std::unique_ptr<AST::Program> parseProgram();
};
