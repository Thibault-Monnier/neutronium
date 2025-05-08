#include <gtest/gtest.h>

#include <lexing/lexer.hpp>
#include <lexing/token_kind.hpp>

struct LexCase {
    std::string src;
    std::vector<TokenKind> kinds;
};

class LexerTokenKindTest : public ::testing::TestWithParam<LexCase> {};

TEST_P(LexerTokenKindTest, ProducesExpectedKinds) {
    Lexer lex(GetParam().src);
    auto toks = lex.tokenize();
    std::vector<TokenKind> got;
    std::transform(toks.begin(), toks.end(), std::back_inserter(got),
                   [](const Token& t) { return t.kind(); });
    EXPECT_EQ(got, GetParam().kinds);
}

INSTANTIATE_TEST_SUITE_P(AllTokens, LexerTokenKindTest,
    ::testing::Values(
        LexCase{"42", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
        LexCase{"true false", {TokenKind::TRUE, TokenKind::FALSE, TokenKind::EOF_}},
        LexCase{"let mut x = 1;",
                {TokenKind::LET, TokenKind::MUT, TokenKind::IDENTIFIER,
                 TokenKind::EQUAL, TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
        LexCase{"# comment\n42", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}}
    )
);

INSTANTIATE_TEST_SUITE_P(AdditionalTokens, LexerTokenKindTest,
    ::testing::Values(
        LexCase{"if elif else while", {TokenKind::IF, TokenKind::ELIF, TokenKind::ELSE, TokenKind::WHILE, TokenKind::EOF_}},
        LexCase{"+ - * / == != < <= > >=", {
            TokenKind::PLUS, TokenKind::MINUS, TokenKind::STAR, TokenKind::SLASH,
            TokenKind::EQUAL_EQUAL, TokenKind::BANG_EQUAL,
            TokenKind::LESS_THAN, TokenKind::LESS_THAN_EQUAL,
            TokenKind::GREATER_THAN, TokenKind::GREATER_THAN_EQUAL,
            TokenKind::EOF_}},
        LexCase{"{ } : ;", {
            TokenKind::LEFT_BRACE, TokenKind::RIGHT_BRACE,
            TokenKind::COLON, TokenKind::SEMICOLON, TokenKind::EOF_}},
        LexCase{"42 15 true false", {
            TokenKind::NUMBER_LITERAL, TokenKind::NUMBER_LITERAL,
            TokenKind::TRUE, TokenKind::FALSE, TokenKind::EOF_}}
    )
);

// ─────────────────────────────────────────────────────────────
// Parens, unary operators, and expression coverage
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(Expressions, LexerTokenKindTest,
    ::testing::Values(
        LexCase{"foo()", {TokenKind::IDENTIFIER, TokenKind::LEFT_PAREN, TokenKind::RIGHT_PAREN, TokenKind::EOF_}},
        LexCase{"(42)", {TokenKind::LEFT_PAREN, TokenKind::NUMBER_LITERAL, TokenKind::RIGHT_PAREN, TokenKind::EOF_}},
        LexCase{"!true", {TokenKind::BANG, TokenKind::TRUE, TokenKind::EOF_}},
        LexCase{"+1", {TokenKind::PLUS, TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
        LexCase{"-0", {TokenKind::MINUS, TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
        LexCase{"x==y", {TokenKind::IDENTIFIER, TokenKind::EQUAL_EQUAL, TokenKind::IDENTIFIER, TokenKind::EOF_}},
        LexCase{"x!=y", {TokenKind::IDENTIFIER, TokenKind::BANG_EQUAL, TokenKind::IDENTIFIER, TokenKind::EOF_}},
        LexCase{"x<=42", {TokenKind::IDENTIFIER, TokenKind::LESS_THAN_EQUAL, TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
        LexCase{"x>=y", {TokenKind::IDENTIFIER, TokenKind::GREATER_THAN_EQUAL, TokenKind::IDENTIFIER, TokenKind::EOF_}}
    )
);

// ─────────────────────────────────────────────────────────────
// Comments, spacing, and full snippet
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(CommentsAndBlocks, LexerTokenKindTest,
    ::testing::Values(
        LexCase{"     # full-line comment\nlet x=1;",
                {TokenKind::LET, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                 TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
        LexCase{"let y = 2;\t# trailing comment\r\n",
                {TokenKind::LET, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                 TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
        LexCase{
            "fn main: {\n"
            "  let mut x = 1;\n"
            "  while x < 10: {\n"
            "    x = x + 1;\n"
            "  }\n"
            "  exit 0;\n"
            "}",
            {
                TokenKind::FN, TokenKind::IDENTIFIER, TokenKind::COLON, TokenKind::LEFT_BRACE,
                TokenKind::LET, TokenKind::MUT, TokenKind::IDENTIFIER,
                TokenKind::EQUAL, TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON,
                TokenKind::WHILE, TokenKind::IDENTIFIER, TokenKind::LESS_THAN,
                TokenKind::NUMBER_LITERAL, TokenKind::COLON, TokenKind::LEFT_BRACE,
                TokenKind::IDENTIFIER, TokenKind::EQUAL, TokenKind::IDENTIFIER,
                TokenKind::PLUS, TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON,
                TokenKind::RIGHT_BRACE,
                TokenKind::EXIT, TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON,
                TokenKind::RIGHT_BRACE,
                TokenKind::EOF_}
        }
    )
);

// ─────────────────────────────────────────────────────────────
// Keyword-prefix edge cases, tight-packed input, long numbers
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(EdgeCases, LexerTokenKindTest,
    ::testing::Values(
        LexCase{"trueval", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
        LexCase{"elseif", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
        LexCase{"fnx", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
        LexCase{"a=1+2*3<=b;", {
            TokenKind::IDENTIFIER, TokenKind::EQUAL,
            TokenKind::NUMBER_LITERAL, TokenKind::PLUS, TokenKind::NUMBER_LITERAL,
            TokenKind::STAR, TokenKind::NUMBER_LITERAL,
            TokenKind::LESS_THAN_EQUAL, TokenKind::IDENTIFIER,
            TokenKind::SEMICOLON, TokenKind::EOF_}},
        LexCase{"00042", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
        LexCase{"18446744073709551615", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}}
    )
);

// ─────────────────────────────────────────────────────────────
// Full coverage with all TokenKinds (except EOF)
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(EverythingOnce, LexerTokenKindTest,
    ::testing::Values(
        LexCase{
            "true false let mut if elif else while fn exit "
            "+ - * / = == != < <= > >= ( ) { } : ; 0 foo1Bar2",
            {
                TokenKind::TRUE, TokenKind::FALSE,
                TokenKind::LET, TokenKind::MUT, TokenKind::IF, TokenKind::ELIF,
                TokenKind::ELSE, TokenKind::WHILE, TokenKind::FN, TokenKind::EXIT,
                TokenKind::PLUS, TokenKind::MINUS, TokenKind::STAR, TokenKind::SLASH,
                TokenKind::EQUAL, TokenKind::EQUAL_EQUAL, TokenKind::BANG_EQUAL,
                TokenKind::LESS_THAN, TokenKind::LESS_THAN_EQUAL,
                TokenKind::GREATER_THAN, TokenKind::GREATER_THAN_EQUAL,
                TokenKind::LEFT_PAREN, TokenKind::RIGHT_PAREN,
                TokenKind::LEFT_BRACE, TokenKind::RIGHT_BRACE,
                TokenKind::COLON, TokenKind::SEMICOLON,
                TokenKind::NUMBER_LITERAL, TokenKind::IDENTIFIER,
                TokenKind::EOF_
            }
        }
    )
);

// ─────────────────────────────────────────────────────────────
// Unexpected token (invalid input causes exit)
// ─────────────────────────────────────────────────────────────
TEST(LexerErrorTest, UnexpectedCharactersCauseExit) {
    std::vector<std::string> badInputs = {"@", "$", "~", "let x = 1 + @`", "x$"};

    for (const auto& input : badInputs) {
        EXPECT_EXIT({
            Lexer lexer(input);
            lexer.tokenize();
        },
        ::testing::ExitedWithCode(EXIT_FAILURE),
        "Invalid character at index");
    }
}

