#pragma once

#include <optional>
#include <string>
#include <vector>

#include "lexing/token.hpp"

class Lexer {
   public:
    explicit Lexer(std::string source) : source_(std::move(source)) {}

    [[nodiscard]] std::vector<Token> tokenize();

   private:
    size_t currentIndex_ = 0;
    const std::string source_;
    std::string buffer_;

    std::vector<Token> tokens_;

    [[nodiscard]] bool is_at_end() const;
    [[nodiscard]] char peek() const;
    char advance();

    void read_to_buffer_while(auto predicate);

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
