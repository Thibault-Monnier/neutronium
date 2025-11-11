#pragma once

#include <optional>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"

class Lexer {
   public:
    explicit Lexer(const std::string_view sourceCode, DiagnosticsEngine& diagnosticsEngine)
        : diagnosticsEngine_(diagnosticsEngine), sourceCode_(sourceCode) {}

    [[nodiscard]] std::vector<Token> tokenize();

   private:
    DiagnosticsEngine& diagnosticsEngine_;

    const std::string_view sourceCode_;
    size_t currentIndex_ = 0;

    std::size_t tokenStartIndex_ = 0;

    std::vector<Token> tokens_;

    /** Estimate of the number of tokens in the source code, using a low estimate of average token
     * size. This should be used to reserve space in the token vector before lexing begins.
     */
    [[nodiscard]] int nbTokensLowEstimate() const;
    /** Estimate of the number of tokens in the source code, using the average token size so far.
     * This should be used to reserve memory for the token vector during lexing if needed. We should
     * wait a little before using this function to have a more accurate estimate.
     */
    [[nodiscard]] int nbTokensEstimate() const;

    void tokenStart() { tokenStartIndex_ = currentIndex_; }

    [[nodiscard]] bool isAtEnd() const;

    [[nodiscard]] char peek() const;
    void advance() { currentIndex_++; }
    /** Peeks the current character and advances the current index by one. Then returns the
     * character.
     */
    [[nodiscard]] char peekAndAdvance();

    [[nodiscard]] std::string_view currentLexeme() const {
        return {sourceCode_.data() + tokenStartIndex_, currentIndex_ - tokenStartIndex_};
    }
    void createToken(TokenKind kind);

    /** Skips to the first character of the next line, or to the end of the source code if there is
     * no other line.
     */
    void skipToNextLine();
    void advanceWhile(auto predicate);

    [[nodiscard]] std::optional<TokenKind> getKeywordKind() const;
    void lexIdentifierContinuation();

    [[nodiscard]] TokenKind lexMinus();
    template <TokenKind singleCharKind, TokenKind twoCharsKind, char otherChar>
    [[nodiscard]] TokenKind lexOpMaybeTwoChars();

    void lexNextChar(char c);
};
