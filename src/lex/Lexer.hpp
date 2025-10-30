#pragma once

#include <optional>
#include <string>
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

    std::string buffer_;

    std::vector<Token> tokens_;

    [[nodiscard]] bool isAtEnd() const;
    [[nodiscard]] char peek() const;
    char advance();

    void createToken(TokenKind kind);

    void advanceWhile(auto predicate);

    [[nodiscard]] std::optional<TokenKind> getKeywordKind() const;
    void lexPlus();
    void lexMinus();
    void lexStar();
    void lexSlash();

    void lexEqual();
    void lexLessThan();
    void lexGreaterThan();
    void lexBang();
};
