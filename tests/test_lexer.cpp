#include <gtest/gtest.h>

#include <string>

#include "lexing/lexer.hpp"
#include "lexing/token.hpp"
#include "lexing/token_kind.hpp"

TEST(LexerTest, SimpleTokenize) {
    std::string input = "let x tO BE1 42j";
    Lexer lexer(input);
    std::vector<Token> tokens = lexer.tokenize();

    ASSERT_FALSE(tokens.empty());
    EXPECT_EQ(tokens[0].kind(), TokenKind::IDENTIFIER);
    EXPECT_EQ(tokens[0].lexeme(), "let");
}
