#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lexing/token_kind.hpp"

namespace AST {

enum class Operator : uint8_t {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    UNDEFINED_OPERATOR,
};

enum class NodeKind : uint8_t {
    NUMBER_LITERAL,
    IDENTIFIER,
    UNARY_EXPRESSION,
    BINARY_EXPRESSION,
    ASSIGNMENT,
    EXIT,
    PROGRAM,
};

struct Node {
    explicit Node(NodeKind kind) : kind_(kind) {}
    virtual ~Node() = default;

    const NodeKind kind_;
};

struct Expression : Node {
    using Node::Node;
};

struct PrimaryExpression : Expression {
    using Expression::Expression;
};

struct NumberLiteral : PrimaryExpression {
    NumberLiteral(int value) : PrimaryExpression{NodeKind::NUMBER_LITERAL}, value_(value) {}

    int value_;
};

struct Identifier : PrimaryExpression {
    Identifier(std::string name)
        : PrimaryExpression{NodeKind::IDENTIFIER}, name_(std::move(name)) {}

    std::string name_;
};

struct UnaryExpression : Expression {
    UnaryExpression(Operator op, std::unique_ptr<Expression> operand)
        : Expression{NodeKind::UNARY_EXPRESSION}, operator_(op), operand_(std::move(operand)) {}

    Operator operator_;
    std::unique_ptr<Expression> operand_;
};

struct BinaryExpression : Expression {
    BinaryExpression(std::unique_ptr<Expression> left, Operator op,
                     std::unique_ptr<Expression> right)
        : Expression{NodeKind::BINARY_EXPRESSION},
          left_(std::move(left)),
          operator_(op),
          right_(std::move(right)) {}

    std::unique_ptr<Expression> left_;
    Operator operator_;
    std::unique_ptr<Expression> right_;
};

struct Statement : Node {
    using Node::Node;
};

struct Assignment final : Statement {
    Assignment(std::string identifier, std::unique_ptr<Expression> value)
        : Statement{NodeKind::ASSIGNMENT},
          identifier_(std::move(identifier)),
          value_(std::move(value)) {}

    std::string identifier_;
    std::unique_ptr<Expression> value_;
};

struct Exit final : Statement {
    Exit(std::unique_ptr<Expression> exitCode)
        : Statement{NodeKind::EXIT}, exitCode_(std::move(exitCode)) {}

    std::unique_ptr<Expression> exitCode_;
};

struct Program final : Node {
    Program() : Node{NodeKind::PROGRAM} {}

    void append_statement(std::unique_ptr<Statement> statement) {
        statements_.emplace_back(std::move(statement));
    }

    std::vector<std::unique_ptr<Statement>> statements_;
};

std::string node_kind_to_string(NodeKind kind);
Operator token_kind_to_AST_operator(const TokenKind tokenKind);
std::string operator_to_string(Operator op);

void log_expression(const Expression& expr, const std::string& prefix, bool isLast);
void log_ast(const Program& node, const std::string& prefix = "", bool isLast = true);

}  // namespace AST
