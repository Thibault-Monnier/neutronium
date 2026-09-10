#pragma once

#include <cstring>
#include <functional>
#include <initializer_list>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include "frontend/ast/AST.hpp"
#include "frontend/ast/Operator.hpp"
#include "frontend/diagnostics/DiagnosticsEngine.hpp"
#include "frontend/lex/Lexer.hpp"
#include "frontend/lex/Token.hpp"
#include "frontend/lex/TokenKind.hpp"
#include "frontend/source/FileID.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeID.hpp"
#include "frontend/type/TypeManager.hpp"
#include "lib/PolymorphicArenaAllocator.hpp"
#include "lib/SmallVector.hpp"

struct ParsedFunctionSignature {
    AST::Identifier* identifier_;
    neutro::SmallVector<AST::VariableDefinition*> parameters_;
    TypeID returnTypeID_;
};

class Parser {
   public:
    explicit Parser(DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                    const std::string_view sourceCode, neutro::PolymorphicArenaAllocator& astArena,
                    TypeManager& typeManager)
        : lexer_(sourceCode, diagnosticsEngine, fileID),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          sourceCode_(sourceCode),
          astArena_(astArena),
          typeManager_(typeManager) {
        token_ = lexer_.lex();
    }

    [[nodiscard]] AST::CompilationUnit* parse();

   private:
    Lexer lexer_;

    DiagnosticsEngine& diagnosticsEngine_;
    FileID fileID_;
    std::string_view sourceCode_;

    neutro::PolymorphicArenaAllocator& astArena_;

    TypeManager& typeManager_;

    // --------------
    // Error handling
    // --------------

    struct [[nodiscard]] ErrorSentinel {
        template <typename T>
        operator T*() const {
            return nullptr;
        }

        template <typename T>
        operator std::optional<T>() const {
            return std::nullopt;
        }

        operator bool() const { return false; }
    };

    /// Emits an error with the given message at the given location.
    void emitError(const std::string& errorMessage, const uint32_t byteOffsetStart,
                   const uint32_t byteOffsetEnd) const {
        diagnosticsEngine_.reportError(errorMessage, byteOffsetStart, byteOffsetEnd, fileID_);
    }

    /// Emits an error with the given message at the location of the given token.
    void emitError(const std::string& errorMessage, const Token& token) const {
        emitError(errorMessage, token.byteOffsetStart(), token.byteOffsetEnd());
    }

    /** Emits an error and returns nullptr of the template type.
     * @param errorMessage The error message to emit.
     * @param token The token where the error occurred.
     * @tparam T The type of the nullptr to return.
     * @return nullptr of type T.
     */
    template <class T>
    [[nodiscard]] ErrorSentinel emitError(const std::string& errorMessage,
                                          const Token& token) const {
        emitError(errorMessage, token);
        return {};
    }
    /** Emits an error at the current token and returns nullptr of the template type.
     * @param errorMessage The error message to emit.
     * @tparam T The type of the nullptr to return.
     * @return nullptr of type T.
     */
    template <class T>
    [[nodiscard]] ErrorSentinel emitError(const std::string& errorMessage) const {
        return emitError<T>(errorMessage, peek());
    }

    void expectError(TokenKind expected) const;
    [[nodiscard]] std::optional<TypeID> invalidTypeSpecifierError() const;
    [[nodiscard]] AST::Expression* invalidPrimaryExpressionError() const;
    [[nodiscard]] AST::NumberLiteral* invalidNumberLiteralError(const Token& token) const;
    [[nodiscard]] AST::CharacterLiteral* invalidEscapeSequenceError(uint32_t byteOffsetStart,
                                                                    uint32_t byteOffsetEnd) const;
    [[nodiscard]] AST::CharacterLiteral* forbiddenCharacterLiteralError(const Token& token) const;
    [[nodiscard]] AST::CharacterLiteral* invalidCharacterLiteralSizeError(const Token& token) const;

    // ---------------
    // Parsing helpers
    // ---------------

    Token token_ = Token::dummy();

    [[nodiscard]] Token peek() const { return token_; }
    void advance() { token_ = lexer_.lex(); }
    inline bool advanceIf(TokenKind expected);
    inline bool expect(TokenKind expected, Token& outToken);

    /** Registers a new type variable in the TypeManager.
     * @return The TypeID of the newly created Type.
     */
    [[nodiscard]] TypeID generateTypeVariable() const { return typeManager_.createTypeVariable(); }

    template <class T>
    std::optional<neutro::SmallVector<T*>> parseCommaSeparatedList(
        TokenKind endDelimiter, const std::function<T*()>& parseElement);
    std::optional<neutro::SmallVector<AST::Expression*>> parseExpressionList(
        TokenKind endDelimiter);

    static std::optional<Type> tryParsePrimitiveType(TokenKind tokenKind);
    std::optional<TypeID> parseTypeSpecifier();

    AST::NumberLiteral* parseNumberLiteral();
    AST::CharacterLiteral* parseCharacterLiteral();
    AST::Expression* parseArrayLiteral();

    AST::Identifier* parseIdentifier();
    AST::Expression* parseIdentifierOrFunctionCall();
    AST::ArrayAccess* parseArrayAccess(const AST::Expression* base);
    AST::Expression* parsePrimaryExpression();
    AST::Expression* parsePostfixExpression();
    AST::Expression* parseUnaryExpression();
    template <AST::Expression* (Parser::*ParseOperandFunc)(), bool AllowMultiple,
              AST::Operator... AllowedOps>
    AST::Expression* parseBinaryExpression();
    AST::Expression* parseMultiplicativeExpression();
    AST::Expression* parseAdditiveExpression();
    AST::Expression* parseComparisonExpression();
    AST::Expression* parseLogicalExpression();
    AST::Expression* parseExpression();

    AST::VariableDefinition* parseVariableDefinition();
    AST::Statement* parseAssignmentOrExpressionStatement();

    AST::BlockStatement* parseElseClause();
    AST::IfStatement* parseIfOrElif(TokenKind kind);
    AST::IfStatement* parseIfStatement();
    AST::WhileStatement* parseWhileStatement();

    AST::VariableDefinition* parseFunctionParameter();
    AST::BreakStatement* parseBreakStatement();
    AST::ContinueStatement* parseContinueStatement();
    AST::ReturnStatement* parseReturnStatement();
    AST::ExitStatement* parseExitStatement();
    AST::BlockStatement* parseBlockStatement();

    AST::Statement* parseStatement();

    bool parseFunctionSignature(ParsedFunctionSignature& outSignature);
    AST::ExternalFunctionDeclaration* parseExternalFunctionDeclaration();

    AST::FunctionDefinition* parseFunctionDefinition();
    AST::CompilationUnit* parseCompilationUnit();
};
