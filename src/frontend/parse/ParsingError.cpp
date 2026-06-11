#include <format>
#include <memory>
#include <string>

#include "Parser.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/lex/Token.hpp"
#include "frontend/lex/TokenKind.hpp"
#include "frontend/type/Type.hpp"

__attribute__((noinline, cold)) void Parser::expectError(const TokenKind expected) const {
    const Token& token = peek();

    const std::string errorMessage =
        std::format("Invalid token -> expected {}, got {}", tokenKindToString(expected),
                    tokenKindToString(token.kind()));
    emitError(errorMessage, token);
}

__attribute__((noinline, cold)) std::unique_ptr<Type> Parser::invalidTypeSpecifierError() const {
    const TokenKind tokenKind = peek().kind();

    const std::string errorMessage = std::format("Invalid token -> expected type specifier, got {}",
                                                 tokenKindToString(tokenKind));
    return std::unique_ptr<Type>(emitError<Type>(errorMessage));
}

__attribute__((noinline, cold)) AST::Expression* Parser::invalidPrimaryExpressionError() const {
    const Token& token = peek();

    const std::string errorMessage =
        std::format("Invalid token at beginning of primary expression -> got {}",
                    tokenKindToString(token.kind()));
    return emitError<AST::Expression>(errorMessage);
}

__attribute__((noinline, cold)) AST::NumberLiteral* Parser::invalidNumberLiteralError(
    const Token& token) const {
    const std::string errorMessage = "Invalid number literal";
    return emitError<AST::NumberLiteral>(errorMessage, token);
}

__attribute__((noinline, cold)) AST::CharacterLiteral* Parser::invalidEscapeSequenceError(
    const uint32_t byteOffsetStart, const uint32_t byteOffsetEnd) const {
    const std::string_view str =
        std::string_view(sourceCode_).substr(byteOffsetStart, byteOffsetEnd - byteOffsetStart + 1);
    const std::string errorMessage = std::format("Invalid escape sequence: `{}`", str);

    emitError(errorMessage, byteOffsetStart, byteOffsetEnd);
    return nullptr;
}

__attribute__((noinline, cold)) AST::CharacterLiteral* Parser::forbiddenCharacterLiteralError(
    const Token& token) const {
    assert(token.length() - 2 == 1);  // Length 1 without the quotes
    const char value = token.lexeme(sourceCode_)[1];

    std::string_view regexName;
    if (value == '\n')
        regexName = "\\n";
    else if (value == '\t')
        regexName = "\\t";
    else if (value == '\r')
        regexName = "\\r";
    else
        std::unreachable();

    const std::string errorMessage = std::format("Character must be escaped: `{}`", regexName);
    return emitError<AST::CharacterLiteral>(errorMessage, token);
}

__attribute__((noinline, cold)) AST::CharacterLiteral* Parser::invalidCharacterLiteralSizeError(
    const Token& token) const {
    const uint32_t size = token.length() - 2;  // Ignore the quotes
    assert(size != 1);
    const std::string errorMessage =
        size == 0 ? "Empty character literal" : "Character literal is too long";
    return emitError<AST::CharacterLiteral>(errorMessage, token);
}
