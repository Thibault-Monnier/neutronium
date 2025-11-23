#pragma once

#include <optional>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"

class Lexer {
   public:
    explicit Lexer(const std::string_view sourceCode, DiagnosticsEngine& diagnosticsEngine)
        : diagnosticsEngine_(diagnosticsEngine),
          sourceStart_(sourceCode.data()),
          sourceEnd_(sourceCode.data() + sourceCode.size()),
          sourceSize_(sourceCode.size()),
          currentPtr_(sourceStart_),
          tokenStartPtr_(currentPtr_) {}

    /// Lexes and returns the next token from the source code.
    [[nodiscard]] Token lex();

    /// Lexes the entire source code and returns a vector of all tokens.
    [[nodiscard]] std::vector<Token> tokenize();

   private:
    DiagnosticsEngine& diagnosticsEngine_;

    const char* const sourceStart_;
    const char* const sourceEnd_;
    const size_t sourceSize_;

    const char* currentPtr_;
    const char* tokenStartPtr_;

    Token result_ = Token::dummy();

    void tokenStart() { tokenStartPtr_ = currentPtr_; }
    [[nodiscard]] char peek() const { return *currentPtr_; }
    void advance() { ++currentPtr_; }

    void createTokenError() const;
    void handleNonAsciiChar();
    void invalidCharacterError(char c) const;

    [[nodiscard]] int currentIndex() const { return static_cast<int>(currentPtr_ - sourceStart_); }

    [[nodiscard]] std::string_view currentLexeme() const {
        return {tokenStartPtr_, static_cast<size_t>(currentPtr_ - tokenStartPtr_)};
    }
    inline void createToken(TokenKind kind);

    /** Skips to the first character of the next line, or to the end of the source code if there is
     * no other line.
     */
    inline void skipToNextLine();

    inline void skipWhile(const auto& predicate);

    /** Skips to the next non-whitespace character
     */
    inline void skipWhitespace();
    inline void lexNumberLiteralContinuation();

    [[nodiscard]] std::optional<TokenKind> getKeywordKind() const;
    inline void lexIdentifierContinuation();

    [[nodiscard]] inline TokenKind lexMinus();
    template <TokenKind singleCharKind, TokenKind twoCharsKind, char otherChar>
    [[nodiscard]] inline TokenKind lexOpMaybeTwoChars();

    [[nodiscard]] inline bool lexNextChar(char c);
};
