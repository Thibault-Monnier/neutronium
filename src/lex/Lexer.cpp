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

__attribute__((always_inline)) void Lexer::createToken(const TokenKind kind) {
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

void Lexer::lexPlus() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::PLUS_EQUAL);
    } else {
        createToken(TokenKind::PLUS);
    }
}

void Lexer::lexMinus() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::MINUS_EQUAL);
    } else if (peek() == '>') {
        advance();
        createToken(TokenKind::RIGHT_ARROW);
    } else {
        createToken(TokenKind::MINUS);
    }
}

void Lexer::lexStar() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::STAR_EQUAL);
    } else {
        createToken(TokenKind::STAR);
    }
}

void Lexer::lexSlash() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::SLASH_EQUAL);
    } else {
        createToken(TokenKind::SLASH);
    }
}

void Lexer::lexEqual() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::EQUAL_EQUAL);
    } else {
        createToken(TokenKind::EQUAL);
    }
}

void Lexer::lexLessThan() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::LESS_THAN_EQUAL);
    } else {
        createToken(TokenKind::LESS_THAN);
    }
}

void Lexer::lexGreaterThan() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::GREATER_THAN_EQUAL);
    } else {
        createToken(TokenKind::GREATER_THAN);
    }
}

void Lexer::lexBang() {
    if (peek() == '=') {
        advance();
        createToken(TokenKind::BANG_EQUAL);
    } else {
        createToken(TokenKind::BANG);
    }
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

        tokenStart();
        char c = peekAndAdvance();

        if (static_cast<unsigned char>(c) >= 128) [[unlikely]] {
            diagnosticsEngine_.reportError("Non-ASCII character encountered", currentIndex_ - 1,
                                           currentIndex_ - 1);

            // Skip remaining UTF-8 continuation bytes (10xxxxxx)
            while (!isAtEnd() && (static_cast<unsigned char>(peek()) & 0b1100'0000) == 0b1000'0000)
                advance();

            continue;
        }

        if (isSpace(c)) continue;

        if (c == '#') {
            skipToNextLine();
            continue;
        }

        if (std::isalpha(c)) {
            advanceWhile([](const char ch) { return std::isalnum(ch) || ch == '_'; });
            if (auto keywordKind = getKeywordKind()) {
                createToken(*keywordKind);
            } else {
                createToken(TokenKind::IDENTIFIER);
            }

        } else if (std::isdigit(c)) {
            advanceWhile(isdigit);
            createToken(TokenKind::NUMBER_LITERAL);
        } else if (c == '+') {
            lexPlus();
        } else if (c == '-') {
            lexMinus();
        } else if (c == '*') {
            lexStar();
        } else if (c == '/') {
            lexSlash();
        } else if (c == '!') {
            lexBang();
        } else if (c == '=') {
            lexEqual();
        } else if (c == '<') {
            lexLessThan();
        } else if (c == '>') {
            lexGreaterThan();
        } else if (c == '(') {
            createToken(TokenKind::LEFT_PAREN);
        } else if (c == ')') {
            createToken(TokenKind::RIGHT_PAREN);
        } else if (c == '{') {
            createToken(TokenKind::LEFT_BRACE);
        } else if (c == '}') {
            createToken(TokenKind::RIGHT_BRACE);
        } else if (c == '[') {
            createToken(TokenKind::LEFT_BRACKET);
        } else if (c == ']') {
            createToken(TokenKind::RIGHT_BRACKET);
        } else if (c == ':') {
            createToken(TokenKind::COLON);
        } else if (c == ';') {
            createToken(TokenKind::SEMICOLON);
        } else if (c == ',') {
            createToken(TokenKind::COMMA);
        } else {
            const std::string errorMessage =
                std::format("Invalid character -> got `{}` (ASCII code {}) at beginning of word", c,
                            static_cast<int>(c));
            diagnosticsEngine_.reportError(errorMessage, currentIndex_ - 1, currentIndex_ - 1);
        }
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