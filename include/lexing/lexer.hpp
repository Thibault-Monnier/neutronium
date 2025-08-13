#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lexing/token.hpp"

class Lexer {
   public:
    explicit Lexer(std::string sourceCode, std::string filename)
        : sourceCode_(std::move(sourceCode)), filename_(std::move(filename)) {}

    [[nodiscard]] std::vector<Token> tokenize();

   private:
    const std::string sourceCode_;
    const std::string filename_;
    size_t currentIndex_ = 0;

    int currentLine_ = 1;
    int currentColumn_ = 1;

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
