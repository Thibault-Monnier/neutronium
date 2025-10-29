#pragma once

#include <optional>
#include <string>
#include <vector>

#include "../DiagnosticsEngine.hpp"
#include "Token.hpp"
#include "TokenKind.hpp"

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

    [[nodiscard]] bool is_at_end() const;
    [[nodiscard]] char peek() const;
    char advance();

    void create_token(TokenKind kind);

    void advance_while(auto predicate);

    [[nodiscard]] std::optional<TokenKind> get_keyword_kind() const;
    void lex_plus();
    void lex_minus();
    void lex_star();
    void lex_slash();

    void lex_equal();
    void lex_less_than();
    void lex_greater_than();
    void lex_bang();
};
