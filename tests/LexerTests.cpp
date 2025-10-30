#include <gtest/gtest.h>

#include <algorithm>
#include <filesystem>
#include <fstream>

#include "diagnostics/DiagnosticsEngine.hpp"
#include "lex/Lexer.hpp"
#include "lex/TokenKind.hpp"
#include "source/SourceManager.hpp"

struct LexCase {
    std::string src;
    std::vector<TokenKind> kinds;
};

class LexerTokenKindTest : public ::testing::TestWithParam<LexCase> {};

TEST_P(LexerTokenKindTest, ProducesExpectedKinds) {
    const SourceManager sm;
    DiagnosticsEngine de(sm, 0);

    Lexer lex(GetParam().src, de);
    auto toks = lex.tokenize();
    std::vector<TokenKind> got;
    std::ranges::transform(toks, std::back_inserter(got), [](const Token& t) { return t.kind(); });
    EXPECT_EQ(got, GetParam().kinds);
}

INSTANTIATE_TEST_SUITE_P(
    AllTokens, LexerTokenKindTest,
    ::testing::Values(LexCase{"42", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
                      LexCase{"true false", {TokenKind::TRUE, TokenKind::FALSE, TokenKind::EOF_}},
                      LexCase{
                          "let mut x = 1;",
                          {TokenKind::LET, TokenKind::MUT, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                           TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
                      LexCase{"# comment @\n42", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}}));

INSTANTIATE_TEST_SUITE_P(
    AdditionalTokens, LexerTokenKindTest,
    ::testing::Values(LexCase{"if elif else while",
                              {TokenKind::IF, TokenKind::ELIF, TokenKind::ELSE, TokenKind::WHILE,
                               TokenKind::EOF_}},
                      LexCase{"+ - * / == != < <= > >=",
                              {TokenKind::PLUS, TokenKind::MINUS, TokenKind::STAR, TokenKind::SLASH,
                               TokenKind::EQUAL_EQUAL, TokenKind::BANG_EQUAL, TokenKind::LESS_THAN,
                               TokenKind::LESS_THAN_EQUAL, TokenKind::GREATER_THAN,
                               TokenKind::GREATER_THAN_EQUAL, TokenKind::EOF_}},
                      LexCase{"{ } : ; ,",
                              {TokenKind::LEFT_BRACE, TokenKind::RIGHT_BRACE, TokenKind::COLON,
                               TokenKind::SEMICOLON, TokenKind::COMMA, TokenKind::EOF_}},
                      LexCase{"42 15 true false",
                              {TokenKind::NUMBER_LITERAL, TokenKind::NUMBER_LITERAL,
                               TokenKind::TRUE, TokenKind::FALSE, TokenKind::EOF_}}));

// ─────────────────────────────────────────────────────────────
// Parens, unary operators, and expression coverage
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(
    Expressions, LexerTokenKindTest,
    ::testing::Values(LexCase{"foo_()",
                              {TokenKind::IDENTIFIER, TokenKind::LEFT_PAREN, TokenKind::RIGHT_PAREN,
                               TokenKind::EOF_}},
                      LexCase{"(42)",
                              {TokenKind::LEFT_PAREN, TokenKind::NUMBER_LITERAL,
                               TokenKind::RIGHT_PAREN, TokenKind::EOF_}},
                      LexCase{"!true", {TokenKind::BANG, TokenKind::TRUE, TokenKind::EOF_}},
                      LexCase{"+1,",
                              {TokenKind::PLUS, TokenKind::NUMBER_LITERAL, TokenKind::COMMA,
                               TokenKind::EOF_}},
                      LexCase{"-0", {TokenKind::MINUS, TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
                      LexCase{"x==y",
                              {TokenKind::IDENTIFIER, TokenKind::EQUAL_EQUAL, TokenKind::IDENTIFIER,
                               TokenKind::EOF_}},
                      LexCase{"x!=y",
                              {TokenKind::IDENTIFIER, TokenKind::BANG_EQUAL, TokenKind::IDENTIFIER,
                               TokenKind::EOF_}},
                      LexCase{"x<=42",
                              {TokenKind::IDENTIFIER, TokenKind::LESS_THAN_EQUAL,
                               TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
                      LexCase{"x>=y",
                              {TokenKind::IDENTIFIER, TokenKind::GREATER_THAN_EQUAL,
                               TokenKind::IDENTIFIER, TokenKind::EOF_}}));

// ─────────────────────────────────────────────────────────────
// Comments, spacing, and full snippet
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(
    CommentsAndBlocks, LexerTokenKindTest,
    ::testing::Values(LexCase{"     # full-line comment\nlet x=1;",
                              {TokenKind::LET, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                               TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
                      LexCase{"let y = 2;\t# trailing comment\r\n",
                              {TokenKind::LET, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                               TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
                      LexCase{"fn main: {\n"
                              "  let mut x: int = 1;\n"
                              "  while x < 10: {\n"
                              "    x += 1;\n"
                              "  }\n"
                              "  exit 0;\n"
                              "}",
                              {TokenKind::FN,
                               TokenKind::IDENTIFIER,
                               TokenKind::COLON,
                               TokenKind::LEFT_BRACE,
                               TokenKind::LET,
                               TokenKind::MUT,
                               TokenKind::IDENTIFIER,
                               TokenKind::COLON,
                               TokenKind::INT,
                               TokenKind::EQUAL,
                               TokenKind::NUMBER_LITERAL,
                               TokenKind::SEMICOLON,
                               TokenKind::WHILE,
                               TokenKind::IDENTIFIER,
                               TokenKind::LESS_THAN,
                               TokenKind::NUMBER_LITERAL,
                               TokenKind::COLON,
                               TokenKind::LEFT_BRACE,
                               TokenKind::IDENTIFIER,
                               TokenKind::PLUS_EQUAL,
                               TokenKind::NUMBER_LITERAL,
                               TokenKind::SEMICOLON,
                               TokenKind::RIGHT_BRACE,
                               TokenKind::EXIT,
                               TokenKind::NUMBER_LITERAL,
                               TokenKind::SEMICOLON,
                               TokenKind::RIGHT_BRACE,
                               TokenKind::EOF_}}));

// ─────────────────────────────────────────────────────────────
// Keyword-prefix edge cases, tight-packed input, long numbers
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(
    EdgeCases, LexerTokenKindTest,
    ::testing::Values(LexCase{"trueval", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
                      LexCase{"elseif", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
                      LexCase{"fnx", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
                      LexCase{"a=1+2*3<=b;",
                              {TokenKind::IDENTIFIER, TokenKind::EQUAL, TokenKind::NUMBER_LITERAL,
                               TokenKind::PLUS, TokenKind::NUMBER_LITERAL, TokenKind::STAR,
                               TokenKind::NUMBER_LITERAL, TokenKind::LESS_THAN_EQUAL,
                               TokenKind::IDENTIFIER, TokenKind::SEMICOLON, TokenKind::EOF_}},
                      LexCase{"00042", {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}},
                      LexCase{"18446744073709551615",
                              {TokenKind::NUMBER_LITERAL, TokenKind::EOF_}}));

// ─────────────────────────────────────────────────────────────
// Full coverage with all TokenKinds (except EOF)
// ─────────────────────────────────────────────────────────────
INSTANTIATE_TEST_SUITE_P(
    EverythingOnce, LexerTokenKindTest,
    ::testing::Values(LexCase{
        "true false int bool let mut if elif else continue break while fn extern return exit "
        "+ - * / = += -= *= /= == != < <= > >= ( ) { } [ ] : ; , 0 foo_1Bar2_",
        {TokenKind::TRUE,
         TokenKind::FALSE,
         TokenKind::INT,
         TokenKind::BOOL,
         TokenKind::LET,
         TokenKind::MUT,
         TokenKind::IF,
         TokenKind::ELIF,
         TokenKind::ELSE,
         TokenKind::CONTINUE,
         TokenKind::BREAK,
         TokenKind::WHILE,
         TokenKind::FN,
         TokenKind::EXTERN,
         TokenKind::RETURN,
         TokenKind::EXIT,
         TokenKind::PLUS,
         TokenKind::MINUS,
         TokenKind::STAR,
         TokenKind::SLASH,
         TokenKind::EQUAL,
         TokenKind::PLUS_EQUAL,
         TokenKind::MINUS_EQUAL,
         TokenKind::STAR_EQUAL,
         TokenKind::SLASH_EQUAL,
         TokenKind::EQUAL_EQUAL,
         TokenKind::BANG_EQUAL,
         TokenKind::LESS_THAN,
         TokenKind::LESS_THAN_EQUAL,
         TokenKind::GREATER_THAN,
         TokenKind::GREATER_THAN_EQUAL,
         TokenKind::LEFT_PAREN,
         TokenKind::RIGHT_PAREN,
         TokenKind::LEFT_BRACE,
         TokenKind::RIGHT_BRACE,
         TokenKind::LEFT_BRACKET,
         TokenKind::RIGHT_BRACKET,
         TokenKind::COLON,
         TokenKind::SEMICOLON,
         TokenKind::COMMA,
         TokenKind::NUMBER_LITERAL,
         TokenKind::IDENTIFIER,
         TokenKind::EOF_}}));

// ─────────────────────────────────────────────────────────────
// Unexpected token (invalid input causes exit)
// ─────────────────────────────────────────────────────────────
TEST(LexerErrorTest, UnexpectedCharactersCauseExit) {
    namespace fs = std::filesystem;
    const std::vector<std::string> badInputs = {
        "@", "$", "~", "let x = 1 + @`", "x$", "let _invalid_identifier = 42;"};

    for (const auto& input : badInputs) {
        fs::path tmp = fs::temp_directory_path() / "test_input.nt";
        std::ofstream(tmp) << input;

        EXPECT_EXIT(
            {
                freopen("/dev/null", "w", stdout);

                SourceManager sm;
                const auto fileData = sm.loadNewSourceFile(tmp.string());
                const auto fileID = fileData.first;
                const auto fileContents = fileData.second;
                DiagnosticsEngine de(sm, fileID);
                Lexer lexer(fileContents, de);
                auto _ = lexer.tokenize();
            },
            ::testing::ExitedWithCode(EXIT_FAILURE), "Invalid character");
    }
}

TEST(LexerErrorTest, NonASCIICharactersCauseExit) {
    namespace fs = std::filesystem;
    const std::vector<std::string> badInputs = {"let x = 42π;", "こんにちは", "let привет = 1;",
                                                "x = y + λ;"};

    for (const auto& input : badInputs) {
        fs::path tmp = fs::temp_directory_path() / "test_input.nt";
        std::ofstream(tmp) << input;

        EXPECT_EXIT(
            {
                freopen("/dev/null", "w", stdout);

                SourceManager sm;
                const auto fileData = sm.loadNewSourceFile(tmp.string());
                const auto fileID = fileData.first;
                const auto fileContents = fileData.second;
                DiagnosticsEngine de(sm, fileID);
                Lexer lexer(fileContents, de);
                auto _ = lexer.tokenize();
            },
            ::testing::ExitedWithCode(EXIT_FAILURE), "Non-ASCII character");
    }
}

// ─────────────────────────────────────────────────────────────
// Extra Lexer cases
// ─────────────────────────────────────────────────────────────

INSTANTIATE_TEST_SUITE_P(
    LexingExtras, LexerTokenKindTest,
    ::testing::Values(
        // “===”: should split into == then =
        LexCase{"===", {TokenKind::EQUAL_EQUAL, TokenKind::EQUAL, TokenKind::EOF_}},
        // “+==”: should split into + then ==
        LexCase{"+==", {TokenKind::PLUS_EQUAL, TokenKind::EQUAL, TokenKind::EOF_}},
        // “<=>”: <= then >
        LexCase{"<=>", {TokenKind::LESS_THAN_EQUAL, TokenKind::GREATER_THAN, TokenKind::EOF_}},
        // Alphanumeric run: digits then letters → NUMBER_LITERAL, IDENTIFIER
        LexCase{"123abc", {TokenKind::NUMBER_LITERAL, TokenKind::IDENTIFIER, TokenKind::EOF_}},
        // Keyword glued to digit → IDENTIFIER
        LexCase{"exit0", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
        // Keyword-prefix with letter → IDENTIFIER
        LexCase{"whilex", {TokenKind::IDENTIFIER, TokenKind::EOF_}},
        // No space between ‘let’ and ‘mut’ → IDENTIFIER “letmut”
        LexCase{"letmut x=1;",
                {TokenKind::IDENTIFIER, TokenKind::IDENTIFIER, TokenKind::EQUAL,
                 TokenKind::NUMBER_LITERAL, TokenKind::SEMICOLON, TokenKind::EOF_}},
        // Empty input → just EOF
        LexCase{"", {TokenKind::EOF_}},
        // Only whitespace → EOF
        LexCase{"  \n \n\t  ", {TokenKind::EOF_}},
        // Multiple semicolons → SEMICOLON, SEMICOLON, EOF
        LexCase{";;", {TokenKind::SEMICOLON, TokenKind::SEMICOLON, TokenKind::EOF_}},
        //  Trailing empty statements
        LexCase{"let x=1;;",
                {TokenKind::LET, TokenKind::IDENTIFIER, TokenKind::EQUAL, TokenKind::NUMBER_LITERAL,
                 TokenKind::SEMICOLON, TokenKind::SEMICOLON, TokenKind::EOF_}},
        // 12) Single unary ‘!’
        LexCase{"!", {TokenKind::BANG, TokenKind::EOF_}}));
