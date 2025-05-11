#pragma once

#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "lexing/token.hpp"
#include "parsing/ast.hpp"

class Parser {
   public:
    explicit Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    [[nodiscard]] AST::Program parse();

   private:
    std::vector<Token> tokens_;
    size_t currentIndex_ = 0;

    [[noreturn]] static void abort(const std::string& errorMessage, const std::string& hintMessage = "");
    [[nodiscard]] const Token& peek(const int amount = 0) const;
    const Token& consume(const TokenKind expected);

    Type parse_type_specifier();

    std::unique_ptr<AST::Identifier> parse_identifier();
    std::unique_ptr<AST::FunctionCall> parse_function_call();

    std::unique_ptr<AST::Expression> parse_primary_expression();
    std::unique_ptr<AST::Expression> parse_unary_expression();
    std::unique_ptr<AST::Expression> parse_binary_expression(
        const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
        const std::set<AST::Operator>& allowedOps, const bool allowMultiple);
    std::unique_ptr<AST::Expression> parse_multiplicative_expression();
    std::unique_ptr<AST::Expression> parse_additive_expression();
    std::unique_ptr<AST::Expression> parse_comparison_expression();
    std::unique_ptr<AST::Expression> parse_expression();

    std::unique_ptr<AST::ExpressionStatement> parse_expression_statement();
    std::unique_ptr<AST::VariableAssignment> parse_variable_assignment();
    std::unique_ptr<AST::VariableDeclaration> parse_variable_declaration();
    std::unique_ptr<AST::IfStatement> parse_if_statement();
    std::unique_ptr<AST::WhileStatement> parse_while_statement();
    std::unique_ptr<AST::FunctionDeclaration> parse_function_declaration();
    std::unique_ptr<AST::BreakStatement> parse_break_statement();
    std::unique_ptr<AST::ContinueStatement> parse_continue_statement();
    std::unique_ptr<AST::Exit> parse_exit();

    std::unique_ptr<AST::BlockStatement> parse_block_statement();
    std::unique_ptr<AST::Statement> parse_statement();
};
