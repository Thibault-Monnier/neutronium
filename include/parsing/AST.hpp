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
    LOGICAL_NOT,
    EQUALS,
    NOT_EQUALS,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    GREATER_THAN,
    GREATER_THAN_OR_EQUAL,
    UNDEFINED_OPERATOR,
};

enum class NodeKind : uint8_t {
    NUMBER_LITERAL,
    BOOLEAN_LITERAL,
    IDENTIFIER,
    UNARY_EXPRESSION,
    BINARY_EXPRESSION,
    ASSIGNMENT,
    IF_STATEMENT,
    EXIT,
    PROGRAM,
};

struct Node {
    explicit Node(const NodeKind kind) : kind_(kind) {}
    virtual ~Node() = default;

    const NodeKind kind_;
};

struct Expression : Node {
    using Node::Node;
};

struct PrimaryExpression : Expression {
    using Expression::Expression;
};

struct NumberLiteral final : PrimaryExpression {
    explicit NumberLiteral(const int value)
        : PrimaryExpression{NodeKind::NUMBER_LITERAL}, value_(value) {}

    const int value_;
};

struct BooleanLiteral final : PrimaryExpression {
    explicit BooleanLiteral(const bool value)
        : PrimaryExpression{NodeKind::BOOLEAN_LITERAL}, value_(value) {}

    const bool value_;
};

struct Identifier final : PrimaryExpression {
    explicit Identifier(std::string name)
        : PrimaryExpression{NodeKind::IDENTIFIER}, name_(std::move(name)) {}

    const std::string name_;
};

struct UnaryExpression final : Expression {
    UnaryExpression(Operator op, std::unique_ptr<Expression> operand)
        : Expression{NodeKind::UNARY_EXPRESSION}, operator_(op), operand_(std::move(operand)) {}

    const Operator operator_;
    const std::unique_ptr<Expression> operand_;
};

struct BinaryExpression final : Expression {
    BinaryExpression(std::unique_ptr<Expression> left, Operator op,
                     std::unique_ptr<Expression> right)
        : Expression{NodeKind::BINARY_EXPRESSION},
          left_(std::move(left)),
          operator_(op),
          right_(std::move(right)) {}

    const std::unique_ptr<Expression> left_;
    const Operator operator_;
    const std::unique_ptr<Expression> right_;
};

struct Statement : Node {
    using Node::Node;
};

struct Assignment final : Statement {
    Assignment(std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> value,
               const bool isDeclaration)
        : Statement{NodeKind::ASSIGNMENT},
          identifier_(std::move(identifier)),
          value_(std::move(value)),
          isDeclaration_(isDeclaration) {}

    const std::unique_ptr<Identifier> identifier_;
    const std::unique_ptr<Expression> value_;
    const bool isDeclaration_;
};

struct IfStatement final : Statement {
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body)
        : Statement{NodeKind::IF_STATEMENT},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    const std::unique_ptr<Expression> condition_;
    const std::unique_ptr<Statement> body_;
};

struct Exit final : Statement {
    explicit Exit(std::unique_ptr<Expression> exitCode)
        : Statement{NodeKind::EXIT}, exitCode_(std::move(exitCode)) {}

    const std::unique_ptr<Expression> exitCode_;
};

struct Program final : Node {
    Program() : Node{NodeKind::PROGRAM} {}

    void append_statement(std::unique_ptr<Statement> statement) {
        statements_.emplace_back(std::move(statement));
    }

    std::vector<std::unique_ptr<Statement>> statements_;
};

std::string node_kind_to_string(NodeKind kind);
Operator token_kind_to_operator(TokenKind tokenKind);
std::string operator_to_string(Operator op);

bool is_arithmetic_operator(Operator op);
bool is_equality_operator(Operator op);
bool is_relational_operator(Operator op);
bool is_comparison_operator(Operator op);

void log_expression(const Expression& expr, const std::string& prefix, bool isLast);
void log_statement(const Statement& stmt, const std::string& prefix, bool isLast);
void log_ast(const Program& programNode);

}  // namespace AST
