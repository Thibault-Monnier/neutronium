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

    [[nodiscard]] std::vector<Token> tokenize();

   private:
    DiagnosticsEngine& diagnosticsEngine_;

    const char* const sourceStart_;
    const char* const sourceEnd_;
    const size_t sourceSize_;

    const char* currentPtr_;
    const char* tokenStartPtr_;

    std::vector<Token> tokens_;

    /** Estimate of the number of tokens in the source code. If no tokens have been lexed yet, a
     * default estimate is returned. Otherwise, the estimate is based on the average size of the
     * tokens lexed so far. This should be used to reserve memory for the token vector during lexing
     * if needed.
     */
    [[nodiscard]] int nbTokensEstimate() const;

    void tokenStart() { tokenStartPtr_ = currentPtr_; }

    [[nodiscard]] bool isAtEnd() const;

    [[nodiscard]] char peek() const;
    void advance() { ++currentPtr_; }
    /** Peeks the current character and advances the current index by one. Then returns the
     * character.
     */
    [[nodiscard]] char peekAndAdvance();

    void createTokenError() const;
    void handleNonAsciiChar();
    void invalidCharacterError(char c) const;

    [[nodiscard]] int currentIndex() const { return static_cast<int>(currentPtr_ - sourceStart_); }

    [[nodiscard]] std::string_view currentLexeme() const {
        return {tokenStartPtr_, static_cast<size_t>(currentPtr_ - tokenStartPtr_)};
    }
    void createToken(TokenKind kind);

    /** Skips to the first character of the next line, or to the end of the source code if there is
     * no other line.
     */
    void skipToNextLine();
    /** Skips to the next non-whitespace character
     */
    void skipWhitespace();
    void lexNumberLiteralContinuation();

    [[nodiscard]] std::optional<TokenKind> getKeywordKind() const;
    void lexIdentifierContinuation();

    [[nodiscard]] TokenKind lexMinus();
    template <TokenKind singleCharKind, TokenKind twoCharsKind, char otherChar>
    [[nodiscard]] TokenKind lexOpMaybeTwoChars();

    void lexNextChar(char c);
};
