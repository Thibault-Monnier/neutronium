#include "Lexer.hpp"

#include <format>
#include <optional>
#include <string>
#include <vector>

#include "Token.hpp"
#include "TokenKind.hpp"

bool Lexer::is_at_end() const { return currentIndex_ >= sourceCode_.length(); }

char Lexer::peek() const { return is_at_end() ? '\0' : sourceCode_.at(currentIndex_); }

char Lexer::advance() {
    const char currentChar = peek();
    buffer_.push_back(currentChar);
    currentIndex_++;
    return currentChar;
}

void Lexer::create_token(const TokenKind kind) {
    tokens_.emplace_back(kind, buffer_, currentIndex_ - buffer_.length());
}

void Lexer::advance_while(auto predicate) {
    while (!is_at_end() && predicate(peek())) {
        advance();
    }
}

std::optional<TokenKind> Lexer::get_keyword_kind() const {
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

void Lexer::lex_plus() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::PLUS_EQUAL);
    } else {
        create_token(TokenKind::PLUS);
    }
}

void Lexer::lex_minus() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::MINUS_EQUAL);
    } else if (peek() == '>') {
        advance();
        create_token(TokenKind::RIGHT_ARROW);
    } else {
        create_token(TokenKind::MINUS);
    }
}

void Lexer::lex_star() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::STAR_EQUAL);
    } else {
        create_token(TokenKind::STAR);
    }
}

void Lexer::lex_slash() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::SLASH_EQUAL);
    } else {
        create_token(TokenKind::SLASH);
    }
}

void Lexer::lex_equal() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::EQUAL_EQUAL);
    } else {
        create_token(TokenKind::EQUAL);
    }
}

void Lexer::lex_less_than() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::LESS_THAN_EQUAL);
    } else {
        create_token(TokenKind::LESS_THAN);
    }
}

void Lexer::lex_greater_than() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::GREATER_THAN_EQUAL);
    } else {
        create_token(TokenKind::GREATER_THAN);
    }
}

void Lexer::lex_bang() {
    if (peek() == '=') {
        advance();
        create_token(TokenKind::BANG_EQUAL);
    } else {
        create_token(TokenKind::BANG);
    }
}

std::vector<Token> Lexer::tokenize() {
    while (!is_at_end()) {
        buffer_.clear();
        char c = advance();

        if (std::isspace(c)) continue;

        if (c == '#') {
            while (!is_at_end() && peek() != '\n') advance();
            continue;
        }

        if (std::isalpha(c)) {
            advance_while([](const char ch) { return std::isalnum(ch) || ch == '_'; });
            if (auto keywordKind = get_keyword_kind()) {
                create_token(*keywordKind);
            } else {
                create_token(TokenKind::IDENTIFIER);
            }

        } else if (std::isdigit(c)) {
            advance_while(isdigit);
            create_token(TokenKind::NUMBER_LITERAL);
        } else if (c == '+') {
            lex_plus();
        } else if (c == '-') {
            lex_minus();
        } else if (c == '*') {
            lex_star();
        } else if (c == '/') {
            lex_slash();
        } else if (c == '!') {
            lex_bang();
        } else if (c == '=') {
            lex_equal();
        } else if (c == '<') {
            lex_less_than();
        } else if (c == '>') {
            lex_greater_than();
        } else if (c == '(') {
            create_token(TokenKind::LEFT_PAREN);
        } else if (c == ')') {
            create_token(TokenKind::RIGHT_PAREN);
        } else if (c == '{') {
            create_token(TokenKind::LEFT_BRACE);
        } else if (c == '}') {
            create_token(TokenKind::RIGHT_BRACE);
        } else if (c == '[') {
            create_token(TokenKind::LEFT_BRACKET);
        } else if (c == ']') {
            create_token(TokenKind::RIGHT_BRACKET);
        } else if (c == ':') {
            create_token(TokenKind::COLON);
        } else if (c == ';') {
            create_token(TokenKind::SEMICOLON);
        } else if (c == ',') {
            create_token(TokenKind::COMMA);
        } else {
            const std::string errorMessage =
                std::format("Invalid character -> got `{}` (ASCII code {}) at beginning of word", c,
                            static_cast<int>(c));
            diagnosticsEngine_.report_error(errorMessage, currentIndex_ - 1, currentIndex_ - 1);
        }
    }

    buffer_.clear();
    advance();
    create_token(TokenKind::EOF_);

    if (diagnosticsEngine_.has_errors()) {
        diagnosticsEngine_.emit_errors();
        std::exit(EXIT_FAILURE);
    }

    return tokens_;
}