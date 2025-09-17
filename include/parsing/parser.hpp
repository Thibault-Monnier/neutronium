#pragma once

#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "diagnostics_engine.hpp"
#include "lexing/token.hpp"
#include "parsing/ast.hpp"

struct ParsedFunctionSignature {
    std::unique_ptr<AST::Identifier> identifier_;
    std::vector<std::unique_ptr<AST::VariableDefinition>> parameters_;
    Type returnType_;
};

class Parser {
   public:
    explicit Parser(std::vector<Token> tokens, DiagnosticsEngine& diagnosticsEngine)
        : diagnosticsEngine_(diagnosticsEngine), tokens_(std::move(tokens)) {}

    [[nodiscard]] std::unique_ptr<AST::Program> parse();

   private:
    DiagnosticsEngine& diagnosticsEngine_;

    std::vector<Token> tokens_;
    size_t currentIndex_ = 0;

    [[noreturn]] void abort(const std::string& errorMessage) const;

    [[nodiscard]] const Token& peek(int amount = 0) const;
    const Token& expect(TokenKind expected);

    Type parse_type_specifier();
    std::unique_ptr<AST::NumberLiteral> parse_number_literal();
    std::unique_ptr<AST::ArrayLiteral> parse_array_literal();

    std::unique_ptr<AST::Identifier> parse_identifier();
    std::unique_ptr<AST::FunctionCall> parse_function_call();
    std::unique_ptr<AST::ArrayAccess> parse_array_access(std::unique_ptr<AST::Expression>& base);
    std::unique_ptr<AST::Expression> parse_primary_expression();
    std::unique_ptr<AST::Expression> parse_postfix_expression();
    std::unique_ptr<AST::Expression> parse_unary_expression();
    std::unique_ptr<AST::Expression> parse_binary_expression(
        const std::function<std::unique_ptr<AST::Expression>()>& parseOperand,
        const std::set<AST::Operator>& allowedOps, bool allowMultiple);
    std::unique_ptr<AST::Expression> parse_multiplicative_expression();
    std::unique_ptr<AST::Expression> parse_additive_expression();
    std::unique_ptr<AST::Expression> parse_comparison_expression();
    std::unique_ptr<AST::Expression> parse_expression();

    std::unique_ptr<AST::ExpressionStatement> parse_expression_statement();
    std::unique_ptr<AST::VariableDefinition> parse_variable_definition();
    std::unique_ptr<AST::Assignment> parse_assignment();
    std::unique_ptr<AST::BlockStatement> parse_else_clause();
    std::unique_ptr<AST::IfStatement> parse_if_statement();
    std::unique_ptr<AST::WhileStatement> parse_while_statement();
    std::unique_ptr<AST::VariableDefinition> parse_function_parameter();
    std::unique_ptr<AST::BreakStatement> parse_break_statement();
    std::unique_ptr<AST::ContinueStatement> parse_continue_statement();
    std::unique_ptr<AST::ReturnStatement> parse_return_statement();
    std::unique_ptr<AST::ExitStatement> parse_exit_statement();
    std::unique_ptr<AST::BlockStatement> parse_block_statement();
    std::unique_ptr<AST::Statement> parse_statement();
    ParsedFunctionSignature parse_function_signature();
    std::unique_ptr<AST::ExternalFunctionDeclaration> parse_external_function_declaration();

    std::unique_ptr<AST::FunctionDefinition> parse_function_definition();
    std::unique_ptr<AST::Program> parse_program();
};
