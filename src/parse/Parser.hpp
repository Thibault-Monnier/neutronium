#pragma once

#include <cstring>
#include <functional>
#include <initializer_list>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "ast/AST.hpp"
#include "ast/ASTArena.hpp"
#include "ast/Operator.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Lexer.hpp"
#include "lex/Token.hpp"
#include "lex/TokenKind.hpp"
#include "source/FileID.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

struct ParsedFunctionSignature {
    AST::Identifier* identifier_;
    std::vector<AST::VariableDefinition*> parameters_;
    TypeID returnTypeID_;
};

class Parser {
   public:
    explicit Parser(DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                    const std::string_view sourceCode, ASTArena& astArena, TypeManager& typeManager)
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

    ASTArena& astArena_;

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
    [[nodiscard]] T* emitError(const std::string& errorMessage, const Token token) const {
        emitError(errorMessage, token);
        return nullptr;
    }
    /** Emits an error at the current token and returns nullptr of the template type.
     * @param errorMessage The error message to emit.
     * @tparam T The type of the nullptr to return.
     * @return nullptr of type T.
     */
    template <class T>
    [[nodiscard]] T* emitError(const std::string& errorMessage) const {
        return emitError<T>(errorMessage, peek());
    }

    void expectError(TokenKind expected) const;
    [[nodiscard]] std::unique_ptr<Type> invalidTypeSpecifierError() const;
    [[nodiscard]] AST::Expression* invalidPrimaryExpressionError() const;
    [[nodiscard]] AST::NumberLiteral* invalidNumberLiteralError(const Token& token) const;

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

    /** Inserts a vector into the ASTArena and returns a span to it.
     * @tparam T The type of the elements in the vector.
     * @param vec The vector to insert.
     * @return A span to the inserted vector.
     */
    template <typename T>
        requires std::is_trivially_copyable_v<T>
    [[nodiscard]] std::span<T> insertVector(std::vector<T>&& vec) {
        if (vec.empty()) return {};

        T* data = astArena_.insertArray<T>(vec.size());
        std::memcpy(reinterpret_cast<void*>(data), vec.data(), vec.size() * sizeof(T));
        return {data, vec.size()};
    }

    template <class T>
    std::optional<std::vector<T*>> parseCommaSeparatedList(TokenKind endDelimiter,
                                                           const std::function<T*()>& parseElement);
    std::optional<std::vector<AST::Expression*>> parseExpressionList(TokenKind endDelimiter);

    static std::optional<Type> tryParsePrimitiveType(TokenKind tokenKind);
    std::unique_ptr<Type> parseTypeSpecifier();
    /** Parses a type annotation if the next token matches `typeAnnotationIndicator`, and returns it
     * or std::nullopt if parsing failed.
     * If the next token does not match `typeAnnotationIndicator`, returns `defaultType`.
     */
    std::optional<Type> maybeParseTypeAnnotation(TokenKind typeAnnotationIndicator,
                                                 Type defaultType);

    AST::NumberLiteral* parseNumberLiteral();
    AST::ArrayLiteral* parseArrayLiteral();

    AST::Identifier* parseIdentifier();
    AST::Expression* parseIdentifierOrFunctionCall();
    AST::ArrayAccess* parseArrayAccess(const AST::Expression* base);
    AST::Expression* parsePrimaryExpression();
    AST::Expression* parsePostfixExpression();
    AST::Expression* parseUnaryExpression();
    AST::Expression* parseBinaryExpression(const std::function<AST::Expression*()>& parseOperand,
                                           std::initializer_list<AST::Operator> allowedOps,
                                           bool allowMultiple);
    AST::Expression* parseMultiplicativeExpression();
    AST::Expression* parseAdditiveExpression();
    AST::Expression* parseComparisonExpression();
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

    std::unique_ptr<ParsedFunctionSignature> parseFunctionSignature();
    AST::ExternalFunctionDeclaration* parseExternalFunctionDeclaration();

    AST::FunctionDefinition* parseFunctionDefinition();
    AST::CompilationUnit* parseCompilationUnit();
};
