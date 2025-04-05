#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace AST {

enum Operator : uint8_t {
    UNDEFINED_OPERATOR,
    ADD,
    SUBTRACT,
};

struct ASTNode {
    explicit ASTNode(std::string type) : type_(std::move(type)) {}
    virtual ~ASTNode() = default;

    const std::string type_;
};

struct PrimaryExpression;
struct BinaryExpression;
using Expression =
    std::variant<std::shared_ptr<BinaryExpression>, std::shared_ptr<PrimaryExpression>>;

struct PrimaryExpression : ASTNode {
    PrimaryExpression(int value) : ASTNode{"PrimaryExpression"}, value_(value) {}

    int value_;
};

struct BinaryExpression : ASTNode {
    BinaryExpression() : ASTNode{"Expression"}, operator_(UNDEFINED_OPERATOR) {}
    BinaryExpression(Expression left, Operator op, Expression right)
        : ASTNode{"Expression"}, left_(std::move(left)), operator_(op), right_(std::move(right)) {}

    Expression left_;
    Operator operator_;
    Expression right_;
};

struct Assignment : ASTNode {
    Assignment(std::string identifier, Expression value)
        : ASTNode{"Assignment"}, identifier_(std::move(identifier)), value_(std::move(value)) {}

    std::string identifier_;
    Expression value_;
};

using Statement = std::variant<Assignment, Expression>;
struct Program : ASTNode {
    Program() : ASTNode{"Program"} {}

    std::vector<Statement> statements_;
};

void log_expression(const Expression& expr, int indent = 0);
void log_node(const std::shared_ptr<ASTNode>& node, int indent = 0);

}  // namespace AST