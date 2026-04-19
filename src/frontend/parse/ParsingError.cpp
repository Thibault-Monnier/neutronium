#include <format>
#include <string>

#include "Parser.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/lex/Token.hpp"
#include "frontend/lex/TokenKind.hpp"

void Parser::emitError(const std::string& errorMessage, const Token& token) const {
    diagnosticsEngine_.reportError(errorMessage, token.byteOffsetStart(), token.byteOffsetEnd(),
                                   fileID_);
}

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
