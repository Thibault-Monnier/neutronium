#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lexing/token_kind.hpp"

namespace AST {

enum Operator : uint8_t {
    UNDEFINED_OPERATOR,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
};

enum NodeKind : uint8_t {
    PRIMARY_EXPRESSION,
    BINARY_EXPRESSION,
    ASSIGNMENT,
    PROGRAM,
};

struct ASTNode {
    explicit ASTNode(NodeKind kind) : kind_(kind) {}
    virtual ~ASTNode() = default;

    const NodeKind kind_;
};

struct PrimaryExpression;
struct BinaryExpression;
using Expression =
    std::variant<std::shared_ptr<BinaryExpression>, std::shared_ptr<PrimaryExpression>>;

struct PrimaryExpression : ASTNode {
    PrimaryExpression(int value) : ASTNode{PRIMARY_EXPRESSION}, value_(value) {}

    int value_;
};

struct BinaryExpression : ASTNode {
    BinaryExpression() : ASTNode{BINARY_EXPRESSION}, operator_(UNDEFINED_OPERATOR) {}
    BinaryExpression(Expression left, Operator op, Expression right)
        : ASTNode{BINARY_EXPRESSION},
          left_(std::move(left)),
          operator_(op),
          right_(std::move(right)) {}

    Expression left_;
    Operator operator_;
    Expression right_;
};

struct Assignment : ASTNode {
    Assignment(std::string identifier, Expression value)
        : ASTNode{ASSIGNMENT}, identifier_(std::move(identifier)), value_(std::move(value)) {}

    std::string identifier_;
    Expression value_;
};

using Statement = std::variant<Assignment, Expression>;
struct Program : ASTNode {
    Program() : ASTNode{PROGRAM} {}

    std::vector<Statement> statements_;
};

Operator token_kind_to_AST_operator(const TokenKind tokenKind);
std::string operator_to_string(Operator op);

void log_expression(const Expression& expr, const std::string& prefix, bool isLast);
void log_node(const std::shared_ptr<ASTNode>& node, const std::string& prefix = "", bool isLast = true);

}  // namespace AST