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
    VARIABLE_DEFINITION,
    VARIABLE_ASSIGNMENT,
    EXPRESSION_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    FUNCTION_CALL,
    BREAK_STATEMENT,
    CONTINUE_STATEMENT,
    RETURN_STATEMENT,
    EXIT_STATEMENT,
    BLOCK_STATEMENT,
    CONSTANT_DEFINITION,
    EXTERNAL_FUNCTION_DECLARATION,
    FUNCTION_DEFINITION,
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

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<Expression>> arguments_;
};

struct UnaryExpression final : Expression {
    UnaryExpression(Operator op, std::unique_ptr<Expression> operand)
        : Expression{NodeKind::UNARY_EXPRESSION}, operator_(op), operand_(std::move(operand)) {}

    const Operator operator_;
    std::unique_ptr<Expression> operand_;
};

struct BinaryExpression final : Expression {
    BinaryExpression(std::unique_ptr<Expression> left, Operator op,
                     std::unique_ptr<Expression> right)
        : Expression{NodeKind::BINARY_EXPRESSION},
          left_(std::move(left)),
          operator_(op),
          right_(std::move(right)) {}

    std::unique_ptr<Expression> left_;
    const Operator operator_;
    std::unique_ptr<Expression> right_;
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

struct VariableDefinition final : Statement {
    VariableDefinition(std::unique_ptr<Identifier> identifier, const Type type,
                       const bool isMutable, std::unique_ptr<Expression> value = nullptr)
        : Statement{NodeKind::VARIABLE_DEFINITION},
          identifier_(std::move(identifier)),
          type_(type),
          isMutable_(isMutable),
          value_(std::move(value)) {}

    std::unique_ptr<Identifier> identifier_;
    const Type type_;
    const bool isMutable_;
    std::unique_ptr<Expression> value_;
};

struct VariableAssignment final : Statement {
    VariableAssignment(std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> value)
        : Statement{NodeKind::VARIABLE_ASSIGNMENT},
          identifier_(std::move(identifier)),
          value_(std::move(value)) {}

    std::unique_ptr<Identifier> identifier_;
    std::unique_ptr<Expression> value_;
};

struct ExpressionStatement final : Statement {
    explicit ExpressionStatement(std::unique_ptr<Expression> expression)
        : Statement{NodeKind::EXPRESSION_STATEMENT}, expression_(std::move(expression)) {}

    std::unique_ptr<Expression> expression_;
};

struct IfStatement final : Statement {
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                std::unique_ptr<BlockStatement> elseClause = nullptr)
        : Statement{NodeKind::IF_STATEMENT},
          condition_(std::move(condition)),
          body_(std::move(body)),
          elseClause_(std::move(elseClause)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
    std::unique_ptr<BlockStatement> elseClause_;
};

struct WhileStatement final : Statement {
    WhileStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body)
        : Statement{NodeKind::WHILE_STATEMENT},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
};

struct BreakStatement final : Statement {
    explicit BreakStatement() : Statement{NodeKind::BREAK_STATEMENT} {}
};

struct ContinueStatement final : Statement {
    explicit ContinueStatement() : Statement{NodeKind::CONTINUE_STATEMENT} {}
};

struct ExitStatement final : Statement {
    explicit ExitStatement(std::unique_ptr<Expression> exitCode)
        : Statement{NodeKind::EXIT_STATEMENT}, exitCode_(std::move(exitCode)) {}

    std::unique_ptr<Expression> exitCode_;
};

struct ReturnStatement final : Statement {
    explicit ReturnStatement(std::unique_ptr<Expression> returnValue)
        : Statement{NodeKind::RETURN_STATEMENT}, returnValue_(std::move(returnValue)) {}

    std::unique_ptr<Expression> returnValue_;
};

struct ConstantDefinition final : Node {
    ConstantDefinition(std::unique_ptr<Identifier> identifier, const Type type,
                       std::unique_ptr<Expression> value)
        : Node{NodeKind::CONSTANT_DEFINITION},
          identifier_(std::move(identifier)),
          type_(type),
          value_(std::move(value)) {}

    std::unique_ptr<Identifier> identifier_;
    const Type type_;
    std::unique_ptr<Expression> value_;
};

struct ExternalFunctionDeclaration final : Node {
    ExternalFunctionDeclaration(std::unique_ptr<Identifier> identifier,
                                std::vector<std::unique_ptr<VariableDefinition>> parameters,
                                const Type returnType)
        : Node{NodeKind::EXTERNAL_FUNCTION_DECLARATION},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          returnType_(returnType) {}

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<VariableDefinition>> parameters_;
    const Type returnType_;
};

struct FunctionDefinition final : Node {
    FunctionDefinition(std::unique_ptr<Identifier> identifier,
                       std::vector<std::unique_ptr<VariableDefinition>> parameters,
                       const Type returnType, std::unique_ptr<BlockStatement> body)
        : Node{NodeKind::FUNCTION_DEFINITION},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          returnType_(returnType),
          body_(std::move(body)) {}

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<VariableDefinition>> parameters_;
    const Type returnType_;
    std::unique_ptr<BlockStatement> body_;
};

struct Program final : Node {
    Program() : Node{NodeKind::PROGRAM} {}

    void append_constant(std::unique_ptr<ConstantDefinition> constant) {
        constants_.emplace_back(std::move(constant));
    }

    void append_function(std::unique_ptr<FunctionDefinition> function) {
        functions_.emplace_back(std::move(function));
    }

    void append_extern_function(std::unique_ptr<ExternalFunctionDeclaration> externFunction) {
        externFunctions_.emplace_back(std::move(externFunction));
    }

    std::vector<std::unique_ptr<ConstantDefinition>> constants_;
    std::vector<std::unique_ptr<FunctionDefinition>> functions_;
    std::vector<std::unique_ptr<ExternalFunctionDeclaration>> externFunctions_;
};

Operator token_kind_to_operator(TokenKind tokenKind);
std::string operator_to_string(Operator op);

bool is_arithmetic_operator(Operator op);
bool is_equality_operator(Operator op);
bool is_relational_operator(Operator op);
bool is_comparison_operator(Operator op);

void log_ast(const Program& programNode);

}  // namespace AST
