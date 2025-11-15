#include "Parser.hpp"

void Parser::emitError(const std::string& errorMessage) const {
    const Token& token = peek();
    diagnosticsEngine_.reportError(errorMessage, token.byteOffsetStart(), token.byteOffsetEnd());
}

template <typename T>
std::unique_ptr<T> Parser::emitError(const std::string& errorMessage) const {
    emitError(errorMessage);
    return nullptr;
}

__attribute__((noinline, cold)) void Parser::expectError(const TokenKind expected) const {
    const Token& token = peek();

    const std::string errorMessage =
        std::format("Invalid token -> expected {}, got {}", tokenKindToString(expected),
                    tokenKindToString(token.kind()));
    emitError(errorMessage);
}

__attribute__((noinline, cold)) std::unique_ptr<Type> Parser::parseTypeSpecifierError() const {
    const TokenKind tokenKind = peek().kind();

    const std::string errorMessage = std::format("Invalid token -> expected type specifier, got {}",
                                                 tokenKindToString(tokenKind));
    return emitError<Type>(errorMessage);
}

__attribute__((noinline, cold)) std::unique_ptr<AST::Expression>
Parser::parsePrimaryExpressionError() const {
    const Token& token = peek();

    const std::string errorMessage =
        std::format("Invalid token at beginning of primary expression -> got {}",
                    tokenKindToString(token.kind()));
    return emitError<AST::Expression>(errorMessage);
}

__attribute__((noinline, cold)) std::unique_ptr<AST::Assignment> Parser::parseAssignmentError()
    const {
    const Token& token = peek();

    const std::string errorMessage = std::format(
        "Invalid token -> expected assignment operator, got {}", tokenKindToString(token.kind()));
    return emitError<AST::Assignment>(errorMessage);
}