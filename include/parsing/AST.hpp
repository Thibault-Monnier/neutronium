#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lexing/token_kind.hpp"

namespace AST {

enum class Operator : uint8_t {
    UNDEFINED_OPERATOR,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
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

struct ASTNode {
    explicit ASTNode(NodeKind kind) : kind_(kind) {}
    virtual ~ASTNode() = default;

    const NodeKind kind_;
};

struct NumberLiteral : ASTNode {
    NumberLiteral(int value) : ASTNode{NodeKind::NUMBER_LITERAL}, value_(value) {}

    int value_;
};

struct Identifier : ASTNode {
    Identifier(std::string name) : ASTNode{NodeKind::IDENTIFIER}, name_(std::move(name)) {}

    std::string name_;
};

using PrimaryExpression = std::variant<NumberLiteral, Identifier>;

struct UnaryExpression;
struct BinaryExpression;
using Expression = std::variant<std::shared_ptr<BinaryExpression>, std::shared_ptr<UnaryExpression>,
                                PrimaryExpression>;

struct UnaryExpression : ASTNode {
    UnaryExpression(Operator op, Expression operand)
        : ASTNode{NodeKind::UNARY_EXPRESSION}, operator_(op), operand_(std::move(operand)) {}

    Operator operator_;
    Expression operand_;
};

struct BinaryExpression : ASTNode {
    BinaryExpression() : ASTNode{NodeKind::BINARY_EXPRESSION}, operator_(Operator::UNDEFINED_OPERATOR) {}
    BinaryExpression(Expression left, Operator op, Expression right)
        : ASTNode{NodeKind::BINARY_EXPRESSION},
          left_(std::move(left)),
          operator_(op),
          right_(std::move(right)) {}

    Expression left_;
    Operator operator_;
    Expression right_;
};

struct Assignment : ASTNode {
    Assignment(std::string identifier, Expression value)
        : ASTNode{NodeKind::ASSIGNMENT}, identifier_(std::move(identifier)), value_(std::move(value)) {}

    std::string identifier_;
    Expression value_;
};

struct Exit : ASTNode {
    Exit(Expression exitCode) : ASTNode{NodeKind::EXIT}, exitCode_(std::move(exitCode)) {}

    Expression exitCode_;
};

using Statement = std::variant<Assignment, Exit>;
struct Program : ASTNode {
    Program() : ASTNode{NodeKind::PROGRAM} {}

    std::vector<Statement> statements_;
};

Operator token_kind_to_AST_operator(const TokenKind tokenKind);
std::string operator_to_string(Operator op);

void log_expression(const Expression& expr, const std::string& prefix, bool isLast);
void log_node(const std::shared_ptr<ASTNode>& node, const std::string& prefix = "",
              bool isLast = true);

}  // namespace AST
