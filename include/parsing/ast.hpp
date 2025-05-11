#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lexing/token_kind.hpp"
#include "semantic-analysis/type.hpp"

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
    VARIABLE_DECLARATION,
    VARIABLE_ASSIGNMENT,
    EXPRESSION_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    FUNCTION_CALL,
    FUNCTION_DECLARATION,
    BREAK_STATEMENT,
    CONTINUE_STATEMENT,
    EXIT,
    BLOCK_STATEMENT,
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
    explicit NumberLiteral(const std::int64_t value)
        : PrimaryExpression{NodeKind::NUMBER_LITERAL}, value_(value) {}

    const std::int64_t value_;
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

struct FunctionCall final : PrimaryExpression {
    FunctionCall(std::unique_ptr<Identifier> identifier,
                 std::vector<std::unique_ptr<Expression>> arguments)
        : PrimaryExpression{NodeKind::FUNCTION_CALL},
          identifier_(std::move(identifier)),
          arguments_(std::move(arguments)) {}

    const std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<Expression>> arguments_;
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

struct BlockStatement final : Statement {
    BlockStatement() : Statement{NodeKind::BLOCK_STATEMENT} {}

    void append_statement(std::unique_ptr<Statement> statement) {
        body_.emplace_back(std::move(statement));
    }

    std::vector<std::unique_ptr<Statement>> body_;
};

struct VariableDeclaration final : Statement {
    VariableDeclaration(std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> value,
                        const Type type, const bool isMutable)
        : Statement{NodeKind::VARIABLE_DECLARATION},
          identifier_(std::move(identifier)),
          value_(std::move(value)),
          type_(type),
          isMutable_(isMutable) {}

    const std::unique_ptr<Identifier> identifier_;
    const std::unique_ptr<Expression> value_;
    const Type type_;
    const bool isMutable_;
};

struct VariableAssignment final : Statement {
    VariableAssignment(std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> value)
        : Statement{NodeKind::VARIABLE_ASSIGNMENT},
          identifier_(std::move(identifier)),
          value_(std::move(value)) {}

    const std::unique_ptr<Identifier> identifier_;
    const std::unique_ptr<Expression> value_;
};

struct ExpressionStatement final : Statement {
    explicit ExpressionStatement(std::unique_ptr<Expression> expression)
        : Statement{NodeKind::EXPRESSION_STATEMENT}, expression_(std::move(expression)) {}

    const std::unique_ptr<Expression> expression_;
};

struct IfStatement final : Statement {
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                std::unique_ptr<BlockStatement> elseClause = nullptr)
        : Statement{NodeKind::IF_STATEMENT},
          condition_(std::move(condition)),
          body_(std::move(body)),
          elseClause_(std::move(elseClause)) {}

    const std::unique_ptr<Expression> condition_;
    const std::unique_ptr<BlockStatement> body_;
    std::unique_ptr<BlockStatement> elseClause_;
};

struct WhileStatement final : Statement {
    WhileStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body)
        : Statement{NodeKind::WHILE_STATEMENT},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    const std::unique_ptr<Expression> condition_;
    const std::unique_ptr<BlockStatement> body_;
};

struct FunctionDeclaration final : Statement {
    FunctionDeclaration(std::unique_ptr<Identifier> identifier,
                        std::vector<std::unique_ptr<Identifier>> parameters,
                        std::unique_ptr<BlockStatement> body)
        : Statement{NodeKind::FUNCTION_DECLARATION},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          body_(std::move(body)) {}

    const std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<Identifier>> parameters_;
    const std::unique_ptr<BlockStatement> body_;
};

struct BreakStatement final : Statement {
    explicit BreakStatement() : Statement{NodeKind::BREAK_STATEMENT} {}
};

struct ContinueStatement final : Statement {
    explicit ContinueStatement() : Statement{NodeKind::CONTINUE_STATEMENT} {}
};

struct Exit final : Statement {
    explicit Exit(std::unique_ptr<Expression> exitCode)
        : Statement{NodeKind::EXIT}, exitCode_(std::move(exitCode)) {}

    const std::unique_ptr<Expression> exitCode_;
};

struct Program final : Node {
    Program() : Node{NodeKind::PROGRAM}, body_(std::make_unique<BlockStatement>()) {}

    std::unique_ptr<BlockStatement> body_;
};

Operator token_kind_to_operator(TokenKind tokenKind);
std::string operator_to_string(Operator op);

bool is_arithmetic_operator(Operator op);
bool is_equality_operator(Operator op);
bool is_relational_operator(Operator op);
bool is_comparison_operator(Operator op);

void log_ast(const Program& programNode);

}  // namespace AST
