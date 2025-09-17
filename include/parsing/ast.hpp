#pragma once

#include <memory>
#include <string>
#include <vector>

#include "../semantic-analysis/types/type.hpp"
#include "lexing/token_kind.hpp"

namespace AST {

enum class Operator : uint8_t {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    LOGICAL_NOT,
    ASSIGN,
    ADD_ASSIGN,
    SUBTRACT_ASSIGN,
    MULTIPLY_ASSIGN,
    DIVIDE_ASSIGN,
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
    ARRAY_LITERAL,
    IDENTIFIER,
    ARRAY_ACCESS,
    UNARY_EXPRESSION,
    BINARY_EXPRESSION,
    VARIABLE_DEFINITION,
    ASSIGNMENT,
    EXPRESSION_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    FUNCTION_CALL,
    BREAK_STATEMENT,
    CONTINUE_STATEMENT,
    RETURN_STATEMENT,
    EXIT_STATEMENT,
    BLOCK_STATEMENT,
    EXTERNAL_FUNCTION_DECLARATION,
    FUNCTION_DEFINITION,
    PROGRAM,
};

struct Node {
    Node(const NodeKind kind, const uint32_t sourceStartIndex, const uint32_t sourceEndIndex)
        : kind_(kind), sourceStartIndex_(sourceStartIndex), sourceEndIndex_(sourceEndIndex) {}
    virtual ~Node() = default;

    const NodeKind kind_;

    [[nodiscard]] uint32_t source_start_index() const { return sourceStartIndex_; }
    [[nodiscard]] uint32_t source_end_index() const { return sourceEndIndex_; }

   protected:
    uint32_t sourceStartIndex_;
    uint32_t sourceEndIndex_;
};

struct Expression : Node {
    using Node::Node;
};

struct PrimaryExpression : Expression {
    using Expression::Expression;
};

struct NumberLiteral final : PrimaryExpression {
    NumberLiteral(const std::int64_t value, const uint32_t start, const uint32_t end)
        : PrimaryExpression{NodeKind::NUMBER_LITERAL, start, end}, value_(value) {}

    const std::int64_t value_;
};

struct BooleanLiteral final : PrimaryExpression {
    BooleanLiteral(const bool value, const uint32_t start, const uint32_t end)
        : PrimaryExpression{NodeKind::BOOLEAN_LITERAL, start, end}, value_(value) {}

    const bool value_;
};

struct ArrayLiteral final : PrimaryExpression {
    ArrayLiteral(std::vector<std::unique_ptr<Expression>> elements, const uint32_t start,
                 const uint32_t end)
        : PrimaryExpression{NodeKind::ARRAY_LITERAL, start, end}, elements_(std::move(elements)) {}

    const std::vector<std::unique_ptr<Expression>> elements_;
};

struct Identifier final : PrimaryExpression {
    Identifier(std::string name, const uint32_t start, const uint32_t end)
        : PrimaryExpression{NodeKind::IDENTIFIER, start, end}, name_(std::move(name)) {}

    const std::string name_;
};

struct ArrayAccess final : PrimaryExpression {
    ArrayAccess(std::unique_ptr<Expression> base, std::unique_ptr<Expression> index,
                const uint32_t start, const uint32_t end)
        : PrimaryExpression{NodeKind::ARRAY_ACCESS, start, end},
          base_(std::move(base)),
          index_(std::move(index)) {}

    std::unique_ptr<Expression> base_;
    std::unique_ptr<Expression> index_;
};

struct FunctionCall final : PrimaryExpression {
    FunctionCall(std::unique_ptr<Identifier> callee,
                 std::vector<std::unique_ptr<Expression>> arguments, const uint32_t start,
                 const uint32_t end)
        : PrimaryExpression{NodeKind::FUNCTION_CALL, start, end},
          callee_(std::move(callee)),
          arguments_(std::move(arguments)) {}

    std::unique_ptr<Identifier> callee_;
    const std::vector<std::unique_ptr<Expression>> arguments_;
};

struct UnaryExpression final : Expression {
    UnaryExpression(const Operator op, std::unique_ptr<Expression> operand, const uint32_t start,
                    const uint32_t end)
        : Expression{NodeKind::UNARY_EXPRESSION, start, end},
          operator_(op),
          operand_(std::move(operand)) {}

    const Operator operator_;
    std::unique_ptr<Expression> operand_;
};

struct BinaryExpression final : Expression {
    BinaryExpression(std::unique_ptr<Expression> left, const Operator op,
                     std::unique_ptr<Expression> right, const uint32_t start, const uint32_t end)
        : Expression{NodeKind::BINARY_EXPRESSION, start, end},
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
    explicit BlockStatement(std::vector<std::unique_ptr<Statement>> body, const uint32_t start,
                            const uint32_t end)
        : Statement{NodeKind::BLOCK_STATEMENT, start, end}, body_(std::move(body)) {}

    std::vector<std::unique_ptr<Statement>> body_;
};

struct VariableDefinition final : Statement {
    VariableDefinition(std::unique_ptr<Identifier> identifier, const Type& type,
                       const bool isMutable, const uint32_t start, const uint32_t end)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end},
          identifier_(std::move(identifier)),
          type_(type),
          isMutable_(isMutable) {}

    VariableDefinition(std::unique_ptr<Identifier> identifier, const Type& type,
                       const bool isMutable, std::unique_ptr<Expression> value,
                       const uint32_t start, const uint32_t end)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end},
          identifier_(std::move(identifier)),
          type_(type),
          isMutable_(isMutable),
          value_(std::move(value)) {}

    std::unique_ptr<Identifier> identifier_;
    const Type type_;
    const bool isMutable_;
    std::unique_ptr<Expression> value_;
};

struct Assignment final : Statement {
    Assignment(std::unique_ptr<Expression> place, const Operator op,
               std::unique_ptr<Expression> value, const uint32_t start, const uint32_t end)
        : Statement{NodeKind::ASSIGNMENT, start, end},
          place_(std::move(place)),
          operator_(op),
          value_(std::move(value)) {}

    std::unique_ptr<Expression> place_;
    const Operator operator_;
    std::unique_ptr<Expression> value_;
};

struct ExpressionStatement final : Statement {
    ExpressionStatement(std::unique_ptr<Expression> expression, const uint32_t start,
                        const uint32_t end)
        : Statement{NodeKind::EXPRESSION_STATEMENT, start, end},
          expression_(std::move(expression)) {}

    std::unique_ptr<Expression> expression_;
};

struct IfStatement final : Statement {
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                const uint32_t start, const uint32_t end)
        : Statement{NodeKind::IF_STATEMENT, start, end},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                std::unique_ptr<BlockStatement> elseClause, const uint32_t start,
                const uint32_t end)
        : Statement{NodeKind::IF_STATEMENT, start, end},
          condition_(std::move(condition)),
          body_(std::move(body)),
          elseClause_(std::move(elseClause)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
    std::unique_ptr<BlockStatement> elseClause_;
};

struct WhileStatement final : Statement {
    WhileStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                   const uint32_t start, const uint32_t end)
        : Statement{NodeKind::WHILE_STATEMENT, start, end},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
};

struct BreakStatement final : Statement {
    BreakStatement(const uint32_t start, const uint32_t end)
        : Statement{NodeKind::BREAK_STATEMENT, start, end} {}
};

struct ContinueStatement final : Statement {
    ContinueStatement(const uint32_t start, const uint32_t end)
        : Statement{NodeKind::CONTINUE_STATEMENT, start, end} {}
};

struct ExitStatement final : Statement {
    ExitStatement(std::unique_ptr<Expression> exitCode, const uint32_t start, const uint32_t end)
        : Statement{NodeKind::EXIT_STATEMENT, start, end}, exitCode_(std::move(exitCode)) {}

    std::unique_ptr<Expression> exitCode_;
};

struct ReturnStatement final : Statement {
    ReturnStatement(std::unique_ptr<Expression> returnValue, const uint32_t start,
                    const uint32_t end)
        : Statement{NodeKind::RETURN_STATEMENT, start, end}, returnValue_(std::move(returnValue)) {}

    std::unique_ptr<Expression> returnValue_;
};

struct ExternalFunctionDeclaration final : Node {
    ExternalFunctionDeclaration(std::unique_ptr<Identifier> identifier,
                                std::vector<std::unique_ptr<VariableDefinition>> parameters,
                                const Type& returnType, const uint32_t start, const uint32_t end)
        : Node{NodeKind::EXTERNAL_FUNCTION_DECLARATION, start, end},
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
                       const Type& returnType, const bool isExported,
                       std::unique_ptr<BlockStatement> body, const uint32_t start,
                       const uint32_t end)
        : Node{NodeKind::FUNCTION_DEFINITION, start, end},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          returnType_(returnType),
          isExported_(isExported),
          body_(std::move(body)) {}

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<VariableDefinition>> parameters_;
    const Type returnType_;
    const bool isExported_;
    std::unique_ptr<BlockStatement> body_;
};

struct Program final : Node {
    explicit Program() : Node{NodeKind::PROGRAM, 0, 0} {}

    void append_function(std::unique_ptr<FunctionDefinition> function) {
        functions_.emplace_back(std::move(function));
        sourceEndIndex_ = functions_.back()->source_end_index();
    }

    void append_extern_function(std::unique_ptr<ExternalFunctionDeclaration> externFunction) {
        externalFunctions_.emplace_back(std::move(externFunction));
        sourceEndIndex_ = externalFunctions_.back()->source_end_index();
    }

    std::vector<std::unique_ptr<ExternalFunctionDeclaration>> externalFunctions_;
    std::vector<std::unique_ptr<FunctionDefinition>> functions_;
};

Operator token_kind_to_operator(TokenKind tokenKind);
std::string operator_to_string(Operator op);

bool is_arithmetic_operator(Operator op);
bool is_equality_operator(Operator op);
bool is_relational_operator(Operator op);
bool is_comparison_operator(Operator op);
bool is_assignment_operator(Operator op);

std::string node_kind_to_string(NodeKind kind);

void log_ast(const Program& programNode);

}  // namespace AST
