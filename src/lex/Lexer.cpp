#include "Lexer.hpp"

#include <frozen/string.h>
#include <frozen/unordered_map.h>

#include <cstring>
#include <format>
#include <optional>
#include <string>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"

int Lexer::nbTokensLowEstimate() const {
    // We will probably reserve again later anyway, so use a low estimate to avoid having to
    // move too much memory next time
    return std::max(static_cast<int>(sourceCode_.length() / 8), 16);
}

int Lexer::nbTokensEstimate() const {
    assert(!tokens_.empty());  // Should be called after a few tokens have been lexed

    const auto currentIndex = static_cast<float>(currentIndex_);
    const auto tokenCount = static_cast<float>(tokens_.size());
    const auto sourceSize = static_cast<float>(sourceCode_.size());

    const float averageTokenSize = currentIndex / tokenCount;

    // Avoid reallocation at the end which is very costly
    constexpr float SAFETY_MARGIN = 1.2f;
    return static_cast<int>(sourceSize / averageTokenSize * SAFETY_MARGIN);
}

bool Lexer::isAtEnd() const { return currentIndex_ >= sourceCode_.length(); }

bool Lexer::isSpace(const char c) {
    switch (c) {
        case ' ':
        case '\t':
        case '\n':
        case '\r':
        case '\f':
        case '\v':
            return true;
        default:
            return false;
    }
}

char Lexer::peek() const { return sourceCode_[currentIndex_]; }

char Lexer::peekAndAdvance() {
    const char currentChar = peek();
    advance();
    return currentChar;
}

void Lexer::createToken(const TokenKind kind) {
    tokens_.emplace_back(kind, currentLexeme(), tokenStartIndex_);
}

void Lexer::skipToNextLine() {
    const auto nextNewline = static_cast<const char*>(std::memchr(
        sourceCode_.data() + currentIndex_, '\n', sourceCode_.length() - currentIndex_));

    if (nextNewline)
        // Skip to character after the newline
        currentIndex_ = static_cast<size_t>(nextNewline - sourceCode_.data()) + 1;
    else
        currentIndex_ = sourceCode_.length();  // skip to end if no newline
}

void Lexer::advanceWhile(auto predicate) {
    while (predicate(peek())) {
        advance();
    }
}

std::optional<TokenKind> Lexer::getKeywordKind() const {
    static constexpr auto KEYWORDS = frozen::make_unordered_map<frozen::string, TokenKind>({
        {"true", TokenKind::TRUE},     {"false", TokenKind::FALSE},
        {"int", TokenKind::INT},       {"int8", TokenKind::INT8},
        {"int16", TokenKind::INT16},   {"int32", TokenKind::INT32},
        {"int64", TokenKind::INT64},   {"bool", TokenKind::BOOL},
        {"let", TokenKind::LET},       {"mut", TokenKind::MUT},
        {"if", TokenKind::IF},         {"elif", TokenKind::ELIF},
        {"else", TokenKind::ELSE},     {"while", TokenKind::WHILE},
        {"break", TokenKind::BREAK},   {"continue", TokenKind::CONTINUE},
        {"fn", TokenKind::FN},         {"extern", TokenKind::EXTERN},
        {"export", TokenKind::EXPORT}, {"return", TokenKind::RETURN},
        {"exit", TokenKind::EXIT},
    });

    if (const auto it = KEYWORDS.find(currentLexeme()); it != KEYWORDS.end()) return it->second;
    return std::nullopt;
}

TokenKind Lexer::lexMinus() {
    if (peek() == '=') {
        advance();
        return TokenKind::MINUS_EQUAL;
    } else if (peek() == '>') {
        advance();
        return TokenKind::RIGHT_ARROW;
    } else {
        return TokenKind::MINUS;
    }
}

TokenKind Lexer::lexOpMaybeTwoChars(const TokenKind singleCharKind, const TokenKind twoCharsKind,
                                    const char otherChar = '=') {
    if (peek() == otherChar) {
        advance();
        return twoCharsKind;
    } else {
        return singleCharKind;
    }
}

void Lexer::lexNextChar() {
    tokenStart();
    char c = peekAndAdvance();

    if (static_cast<unsigned char>(c) >= 128) [[unlikely]] {
        diagnosticsEngine_.reportError("Non-ASCII character encountered", currentIndex_ - 1,
                                       currentIndex_ - 1);

        // Skip remaining UTF-8 continuation bytes (10xxxxxx)
        while (!isAtEnd() && (static_cast<unsigned char>(peek()) & 0b1100'0000) == 0b1000'0000)
            advance();

        return;
    }

    TokenKind kind;
    switch (c) {
            // clang-format off
        case ' ': case '\t': case '\n':
        case '\r': case '\f': case '\v':
            // clang-format on

            // Skip whitespace
            return;

        case '#':
            // Comment
            skipToNextLine();
            return;

            // clang-format off
        case 'a': case 'b': case 'c': case 'd': case 'e':
        case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o':
        case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E':
        case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O':
        case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
            // clang-format on

            // Identifier or keyword
            advanceWhile([](const char ch) { return std::isalnum(ch) || ch == '_'; });

            if (const auto keywordKind = getKeywordKind()) {
                kind = *keywordKind;
            } else {
                kind = TokenKind::IDENTIFIER;
            }
            break;

            // clang-format off
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            // clang-format on

            // Number literal
            advanceWhile(isdigit);
            kind = TokenKind::NUMBER_LITERAL;
            break;

        case '+':
            kind = lexOpMaybeTwoChars(TokenKind::PLUS, TokenKind::PLUS_EQUAL, '=');
            break;
        case '-':
            kind = lexMinus();
            break;
        case '*':
            kind = lexOpMaybeTwoChars(TokenKind::STAR, TokenKind::STAR_EQUAL, '=');
            break;
        case '/':
            kind = lexOpMaybeTwoChars(TokenKind::SLASH, TokenKind::SLASH_EQUAL, '=');
            break;
        case '!':
            kind = lexOpMaybeTwoChars(TokenKind::BANG, TokenKind::BANG_EQUAL, '=');
            break;
        case '=':
            kind = lexOpMaybeTwoChars(TokenKind::EQUAL, TokenKind::EQUAL_EQUAL, '=');
            break;
        case '<':
            kind = lexOpMaybeTwoChars(TokenKind::LESS_THAN, TokenKind::LESS_THAN_EQUAL, '=');
            break;
        case '>':
            kind = lexOpMaybeTwoChars(TokenKind::GREATER_THAN, TokenKind::GREATER_THAN_EQUAL, '=');
            break;
        case '(':
            kind = TokenKind::LEFT_PAREN;
            break;
        case ')':
            kind = TokenKind::RIGHT_PAREN;
            break;
        case '{':
            kind = TokenKind::LEFT_BRACE;
            break;
        case '}':
            kind = TokenKind::RIGHT_BRACE;
            break;
        case '[':
            kind = TokenKind::LEFT_BRACKET;
            break;
        case ']':
            kind = TokenKind::RIGHT_BRACKET;
            break;
        case ':':
            kind = TokenKind::COLON;
            break;
        case ';':
            kind = TokenKind::SEMICOLON;
            break;
        case ',':
            kind = TokenKind::COMMA;
            break;

        default: {
            const std::string errorMessage =
                std::format("Invalid character -> got `{}` (ASCII code {}) at beginning of word", c,
                            static_cast<int>(c));
            diagnosticsEngine_.reportError(errorMessage, currentIndex_ - 1, currentIndex_ - 1);
            return;
        }
    }

    createToken(kind);
}

std::vector<Token> Lexer::tokenize() {
    // The lexer relies on the source code being null-terminated
    assert(sourceCode_[sourceCode_.size()] == '\0');

    tokens_.reserve(nbTokensLowEstimate());
    // std::println("Initial token buffer capacity {}", tokens_.capacity());

    while (!isAtEnd()) {
        if (tokens_.size() >= tokens_.capacity()) {
            tokens_.reserve(nbTokensEstimate());
            // std::println("Resized token buffer to capacity {}", tokens_.capacity());
        }
        lexNextChar();
    }

    advance();
    createToken(TokenKind::EOF_);

    if (diagnosticsEngine_.hasErrors()) {
        diagnosticsEngine_.emitErrors();
        std::exit(EXIT_FAILURE);
    }

    // std::println("File size: {} bytes, {} tokens generated", sourceCode_.length(),
    // tokens_.size()); std::println("Final token average size: {:.2} bytes",
    //              static_cast<double>(sourceCode_.length()) /
    //              static_cast<double>(tokens_.size()));

    return tokens_;
}