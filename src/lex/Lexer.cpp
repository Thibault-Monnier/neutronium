#include "Lexer.hpp"

#include <emmintrin.h>
#include <frozen/string.h>
#include <frozen/unordered_map.h>
#include <smmintrin.h>

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <format>
#include <optional>
#include <string>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"

__attribute__((noinline, cold)) void Lexer::createTokenError() const {
    diagnosticsEngine_.reportError("Token length exceeds maximum allowed length",
                                   tokenStartPtr_ - sourceStart_, currentIndex() - 1);
}

__attribute__((noinline, cold)) void Lexer::handleNonAsciiChar() {
    diagnosticsEngine_.reportError("Non-ASCII character encountered", currentIndex() - 1,
                                   currentIndex() - 1);

    const char* const sourceEnd = sourceEnd_;
    const char* currentPtr = currentPtr_;

    // Skip remaining UTF-8 continuation bytes (10xxxxxx)
    while (currentPtr < sourceEnd &&
           (static_cast<unsigned char>(*currentPtr) & 0b1100'0000) == 0b1000'0000)
        currentPtr++;

    currentPtr_ = currentPtr;
}

__attribute__((noinline, cold)) void Lexer::invalidCharacterError(const char c) const {
    const std::string errorMessage =
        std::format("Invalid character -> got `{}` (ASCII code {}) at beginning of word", c,
                    static_cast<int>(c));
    diagnosticsEngine_.reportError(errorMessage, currentIndex() - 1, currentIndex() - 1);
}

__attribute__((always_inline)) void Lexer::createToken(const TokenKind kind) {
    const char* const tokenStartPtr = tokenStartPtr_;

    const auto length = static_cast<uint32_t>(currentPtr_ - tokenStartPtr);
    if (length > UINT16_MAX) {
        createTokenError();
        return;
    }

    result_ = Token(kind, tokenStartPtr - sourceStart_, static_cast<uint16_t>(length));
    hasLexed_ = true;
}

void Lexer::skipToNextLine() {
    const char* currentPtr = currentPtr_;
    const auto nextNewline =
        static_cast<const char*>(std::memchr(currentPtr, '\n', sourceEnd_ - currentPtr));

    if (nextNewline) [[likely]]
        // Skip to character after the newline
        currentPtr_ = nextNewline + 1;
    else
        currentPtr_ = sourceEnd_;  // skip to end if no newline
}

__attribute__((always_inline)) void Lexer::skipWhile(const auto& predicate) {
    const char* ptr = currentPtr_;
    while (predicate(*ptr)) {
        ptr++;
    }
    currentPtr_ = ptr;
}

__attribute__((always_inline)) void Lexer::skipWhitespace() {
    static constexpr auto IS_WHITESPACE = [](const unsigned char c) {
        return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
    };
    skipWhile(IS_WHITESPACE);
}

__attribute__((always_inline)) void Lexer::lexNumberLiteralContinuation() {
    static constexpr auto IS_NUMBER_CHAR = [](const unsigned char c) {
        return c >= '0' && c <= '9';
    };
    skipWhile(IS_NUMBER_CHAR);
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

void Lexer::lexIdentifierContinuation() {
#ifdef __SSE4_2__
    // Use SIMD to check for identifier continuation characters
    // This is highly efficient, as it checks 16 characters at a time

    alignas(16) static constexpr char VALID_RANGES[16] = {'_', '_', 'A', 'Z', 'a', 'z', '0', '9',
                                                          0,   0,   0,   0,   0,   0,   0,   0};
    static constexpr ssize_t BYTES_PER_REG = 16;

    const char* const sourceEnd = sourceEnd_;
    const char* currentPtr = currentPtr_;

    const __m128i validRangesV = _mm_load_si128(reinterpret_cast<const __m128i*>(VALID_RANGES));

    while (sourceEnd - currentPtr >= BYTES_PER_REG) {
        const __m128i charsV = _mm_loadu_si128(reinterpret_cast<const __m128i*>(currentPtr));

        const int consumed = _mm_cmpistri(
            validRangesV, charsV,
            _SIDD_LEAST_SIGNIFICANT | _SIDD_CMP_RANGES | _SIDD_UBYTE_OPS | _SIDD_NEGATIVE_POLARITY);
        currentPtr += consumed;
        if (consumed == BYTES_PER_REG) continue;

        currentPtr_ = currentPtr;
        return;
    }

    currentPtr_ = currentPtr;

#endif

    // Check for identifier continuation characters
    // This uses many comparisons but is well optimized by the compiler
    // It ends up being faster than a lookup table because we don't have to wait for memory
    static constexpr auto IS_IDENTIFIER_CONTINUE_CHAR = [](const unsigned char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
               (c == '_');
    };
    skipWhile(IS_IDENTIFIER_CONTINUE_CHAR);
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

template <TokenKind singleCharKind, TokenKind twoCharsKind, char otherChar>
__attribute__((always_inline)) TokenKind Lexer::lexOpMaybeTwoChars() {
    if (peek() == otherChar) {
        advance();
        return twoCharsKind;
    } else {
        return singleCharKind;
    }
}

__attribute__((always_inline)) void Lexer::lexNextChar(const char c) {
    if (static_cast<unsigned char>(c) >= 128) [[unlikely]] {
        handleNonAsciiChar();
        return;
    }

    TokenKind kind;
    switch (c) {
            // clang-format off
        case ' ': case '\t': case '\n':
        case '\r': case '\f': case '\v':
            // clang-format on

            // Skip whitespace
            skipWhitespace();
            return;

        case '#':
            // Comment
            skipToNextLine();
            return;

        case 'a' ... 'z':
        case 'A' ... 'Z':
            // Identifier or keyword
            lexIdentifierContinuation();

            if (const auto keywordKind = getKeywordKind()) {
                kind = *keywordKind;
            } else {
                kind = TokenKind::IDENTIFIER;
            }
            break;

        case '0' ... '9':
            // Number literal
            lexNumberLiteralContinuation();
            kind = TokenKind::NUMBER_LITERAL;
            break;

        case '+':
            kind = lexOpMaybeTwoChars<TokenKind::PLUS, TokenKind::PLUS_EQUAL, '='>();
            break;
        case '-':
            kind = lexMinus();
            break;
        case '*':
            kind = lexOpMaybeTwoChars<TokenKind::STAR, TokenKind::STAR_EQUAL, '='>();
            break;
        case '/':
            kind = lexOpMaybeTwoChars<TokenKind::SLASH, TokenKind::SLASH_EQUAL, '='>();
            break;
        case '!':
            kind = lexOpMaybeTwoChars<TokenKind::BANG, TokenKind::BANG_EQUAL, '='>();
            break;
        case '=':
            kind = lexOpMaybeTwoChars<TokenKind::EQUAL, TokenKind::EQUAL_EQUAL, '='>();
            break;
        case '<':
            kind = lexOpMaybeTwoChars<TokenKind::LESS_THAN, TokenKind::LESS_THAN_EQUAL, '='>();
            break;
        case '>':
            kind =
                lexOpMaybeTwoChars<TokenKind::GREATER_THAN, TokenKind::GREATER_THAN_EQUAL, '='>();
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

        default:
            invalidCharacterError(c);
            return;
    }

    createToken(kind);
}

Token Lexer::lex() {
    hasLexed_ = false;
    while (!hasLexed_) {
        if (currentPtr_ >= sourceEnd_) [[unlikely]] {
            tokenStart();
            advance();
            createToken(TokenKind::EOF_);
            break;
        }

        tokenStart();
        lexNextChar(*currentPtr_++);
    }

    return result_;
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    tokens.reserve(128);  // Preallocate some space
    while (true) {
        const Token token = lex();
        tokens.push_back(token);
        if (token.kind() == TokenKind::EOF_) break;
    }

    return tokens;
}