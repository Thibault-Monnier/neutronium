#include "Lexer.hpp"

#include <format>
#include <optional>
#include <string>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"

bool Lexer::isAtEnd() const { return currentIndex_ >= sourceCode_.length(); }

char Lexer::peek() const { return isAtEnd() ? '\0' : sourceCode_.at(currentIndex_); }

char Lexer::advance() {
    const char currentChar = peek();
    buffer_.push_back(currentChar);
    currentIndex_++;
    return currentChar;
}

void Lexer::createToken(const TokenKind kind) {
    tokens_.emplace_back(kind, buffer_, currentIndex_ - buffer_.length());
}

void Lexer::advanceWhile(auto predicate) {
    while (!isAtEnd() && predicate(peek())) {
        advance();
    }
}

std::optional<TokenKind> Lexer::getKeywordKind() const {
    if (buffer_ == "true") return TokenKind::TRUE;
    if (buffer_ == "false") return TokenKind::FALSE;
    if (buffer_ == "int") return TokenKind::INT;
    if (buffer_ == "int8") return TokenKind::INT8;
    if (buffer_ == "int16") return TokenKind::INT16;
    if (buffer_ == "int32") return TokenKind::INT32;
    if (buffer_ == "int64") return TokenKind::INT64;
    if (buffer_ == "bool") return TokenKind::BOOL;
    if (buffer_ == "let") return TokenKind::LET;
    if (buffer_ == "mut") return TokenKind::MUT;
    if (buffer_ == "if") return TokenKind::IF;
    if (buffer_ == "elif") return TokenKind::ELIF;
    if (buffer_ == "else") return TokenKind::ELSE;
    if (buffer_ == "while") return TokenKind::WHILE;
    if (buffer_ == "break") return TokenKind::BREAK;
    if (buffer_ == "continue") return TokenKind::CONTINUE;
    if (buffer_ == "fn") return TokenKind::FN;
    if (buffer_ == "extern") return TokenKind::EXTERN;
    if (buffer_ == "export") return TokenKind::EXPORT;
    if (buffer_ == "return") return TokenKind::RETURN;
    if (buffer_ == "exit") return TokenKind::EXIT;

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
    while (!isAtEnd()) {
        buffer_.clear();
        char c = advance();

        if (static_cast<unsigned char>(c) >= 128) {
            diagnosticsEngine_.reportError("Non-ASCII character encountered", currentIndex_ - 1,
                                           currentIndex_ - 1);

            // Skip remaining UTF-8 continuation bytes (10xxxxxx)
            while (!isAtEnd() && (static_cast<unsigned char>(peek()) & 0b1100'0000) == 0b1000'0000)
                advance();

            continue;
        }

        if (std::isspace(c)) continue;

        if (c == '#') {
            while (!isAtEnd() && peek() != '\n') advance();
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

    buffer_.clear();
    advance();
    createToken(TokenKind::EOF_);

    if (diagnosticsEngine_.hasErrors()) {
        diagnosticsEngine_.emitErrors();
        std::exit(EXIT_FAILURE);
    }

    return tokens_;
}