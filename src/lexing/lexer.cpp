#include "lexing/lexer.hpp"

#include <format>
#include <optional>
#include <string>
#include <vector>

#include "lexing/source_location.hpp"
#include "lexing/token.hpp"
#include "utils/log.hpp"

bool Lexer::is_at_end() const { return currentIndex_ >= sourceCode_.length(); }

char Lexer::peek() const { return is_at_end() ? '\0' : sourceCode_.at(currentIndex_); }

char Lexer::advance() {
    const char currentChar = peek();
    buffer_.push_back(currentChar);
    currentIndex_++;
    if (currentChar == '\n') {
        currentLine_++;
        currentColumn_ = 1;
    } else {
        currentColumn_++;
    }
    return currentChar;
}

void Lexer::create_token(const TokenKind kind) {
    const SourceLocation location{currentLine_, currentColumn_, filename_};
    tokens_.emplace_back(kind, buffer_, location);
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
    if (buffer_ == "bool") return TokenKind::BOOL;
    if (buffer_ == "const") return TokenKind::CONST;
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
        char c = advance();
        buffer_ = c;

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
                std::format("Invalid character at index {}, got '{}' at beginning of word",
                            currentIndex_ - 1, c);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }
    }
    create_token(TokenKind::EOF_);
    return tokens_;
}