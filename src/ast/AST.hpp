#pragma once

#include <cstdint>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "Operator.hpp"
#include "lex/TokenKind.hpp"
#include "source/FileID.hpp"
#include "type/TypeID.hpp"

namespace AST {

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
    Node(const NodeKind kind, const uint32_t sourceStartIndex, const uint32_t sourceEndIndex,
         const FileID fileID)
        : kind_(kind),
          sourceStartIndex_(sourceStartIndex),
          sourceEndIndex_(sourceEndIndex),
          fileID_(fileID) {}
    virtual ~Node() = default;

    const NodeKind kind_;

    [[nodiscard]] uint32_t sourceStartIndex() const { return sourceStartIndex_; }
    [[nodiscard]] uint32_t sourceEndIndex() const { return sourceEndIndex_; }
    [[nodiscard]] FileID fileID() const { return fileID_; }

    template <typename T>
    [[nodiscard]] T* as() {
        return static_cast<T*>(this);
    }
    template <typename T>
    [[nodiscard]] const T* as() const {
        return static_cast<const T*>(this);
    }

   protected:
    uint32_t sourceStartIndex_;
    uint32_t sourceEndIndex_;
    FileID fileID_;
};

struct Expression : Node {
    Expression(const NodeKind kind, const uint32_t sourceStartIndex, const uint32_t sourceEndIndex,
               const FileID fileID, const TypeID typeID)
        : Node{kind, sourceStartIndex, sourceEndIndex, fileID}, typeID_(typeID) {}

    TypeID typeID_;
};

struct NumberLiteral final : Expression {
    NumberLiteral(const std::int64_t value, const uint32_t start, const uint32_t end,
                  const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::NUMBER_LITERAL, start, end, fileID, typeID}, value_(value) {}

    const std::int64_t value_;
};

struct BooleanLiteral final : Expression {
    BooleanLiteral(const bool value, const uint32_t start, const uint32_t end, const FileID fileID,
                   const TypeID typeID)
        : Expression{NodeKind::BOOLEAN_LITERAL, start, end, fileID, typeID}, value_(value) {}

    const bool value_;
};

struct ArrayLiteral final : Expression {
    ArrayLiteral(std::vector<std::unique_ptr<Expression>> elements, const uint32_t start,
                 const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::ARRAY_LITERAL, start, end, fileID, typeID},
          elements_(std::move(elements)) {}

    const std::vector<std::unique_ptr<Expression>> elements_;
};

struct Identifier final : Expression {
    Identifier(const std::string_view name, const uint32_t start, const uint32_t end,
               const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::IDENTIFIER, start, end, fileID, typeID}, name_(name) {}

    const std::string_view name_;
};

struct ArrayAccess final : Expression {
    ArrayAccess(std::unique_ptr<Expression> base, std::unique_ptr<Expression> index,
                const uint32_t start, const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::ARRAY_ACCESS, start, end, fileID, typeID},
          base_(std::move(base)),
          index_(std::move(index)) {}

    std::unique_ptr<Expression> base_;
    std::unique_ptr<Expression> index_;
};

struct FunctionCall final : Expression {
    FunctionCall(std::unique_ptr<Identifier> callee,
                 std::vector<std::unique_ptr<Expression>> arguments, const uint32_t start,
                 const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::FUNCTION_CALL, start, end, fileID, typeID},
          callee_(std::move(callee)),
          arguments_(std::move(arguments)) {}

    std::unique_ptr<Identifier> callee_;
    const std::vector<std::unique_ptr<Expression>> arguments_;
};

struct UnaryExpression final : Expression {
    UnaryExpression(const Operator op, std::unique_ptr<Expression> operand, const uint32_t start,
                    const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::UNARY_EXPRESSION, start, end, fileID, typeID},
          operator_(op),
          operand_(std::move(operand)) {}

    const Operator operator_;
    std::unique_ptr<Expression> operand_;
};

struct BinaryExpression final : Expression {
    BinaryExpression(std::unique_ptr<Expression> left, const Operator op,
                     std::unique_ptr<Expression> right, const uint32_t start, const uint32_t end,
                     const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::BINARY_EXPRESSION, start, end, fileID, typeID},
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
                            const uint32_t end, const FileID fileID)
        : Statement{NodeKind::BLOCK_STATEMENT, start, end, fileID}, body_(std::move(body)) {}

    std::vector<std::unique_ptr<Statement>> body_;
};

struct VariableDefinition final : Statement {
    VariableDefinition(std::unique_ptr<Identifier> identifier, const TypeID typeID,
                       const bool isMutable, const uint32_t start, const uint32_t end,
                       const FileID fileID)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end, fileID},
          identifier_(std::move(identifier)),
          typeID_(typeID),
          isMutable_(isMutable) {}

    VariableDefinition(std::unique_ptr<Identifier> identifier, const TypeID typeID,
                       const bool isMutable, std::unique_ptr<Expression> value,
                       const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end, fileID},
          identifier_(std::move(identifier)),
          typeID_(typeID),
          isMutable_(isMutable),
          value_(std::move(value)) {}

    std::unique_ptr<Identifier> identifier_;
    const TypeID typeID_;
    const bool isMutable_;
    std::unique_ptr<Expression> value_;
};

struct Assignment final : Statement {
    Assignment(std::unique_ptr<Expression> place, const Operator op,
               std::unique_ptr<Expression> value, const uint32_t start, const uint32_t end,
               const FileID fileID)
        : Statement{NodeKind::ASSIGNMENT, start, end, fileID},
          place_(std::move(place)),
          operator_(op),
          value_(std::move(value)) {}

    std::unique_ptr<Expression> place_;
    const Operator operator_;
    std::unique_ptr<Expression> value_;
};

struct ExpressionStatement final : Statement {
    ExpressionStatement(std::unique_ptr<Expression> expression, const uint32_t start,
                        const uint32_t end, const FileID fileID)
        : Statement{NodeKind::EXPRESSION_STATEMENT, start, end, fileID},
          expression_(std::move(expression)) {}

    std::unique_ptr<Expression> expression_;
};

struct IfStatement final : Statement {
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                std::unique_ptr<BlockStatement> elseClause, const uint32_t start,
                const uint32_t end, const FileID fileID)
        : Statement{NodeKind::IF_STATEMENT, start, end, fileID},
          condition_(std::move(condition)),
          body_(std::move(body)),
          elseClause_(std::move(elseClause)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
    std::unique_ptr<BlockStatement> elseClause_;
};

struct WhileStatement final : Statement {
    WhileStatement(std::unique_ptr<Expression> condition, std::unique_ptr<BlockStatement> body,
                   const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::WHILE_STATEMENT, start, end, fileID},
          condition_(std::move(condition)),
          body_(std::move(body)) {}

    std::unique_ptr<Expression> condition_;
    std::unique_ptr<BlockStatement> body_;
};

struct BreakStatement final : Statement {
    BreakStatement(const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::BREAK_STATEMENT, start, end, fileID} {}
};

struct ContinueStatement final : Statement {
    ContinueStatement(const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::CONTINUE_STATEMENT, start, end, fileID} {}
};

struct ExitStatement final : Statement {
    ExitStatement(std::unique_ptr<Expression> exitCode, const uint32_t start, const uint32_t end,
                  const FileID fileID)
        : Statement{NodeKind::EXIT_STATEMENT, start, end, fileID}, exitCode_(std::move(exitCode)) {}

    std::unique_ptr<Expression> exitCode_;
};

struct ReturnStatement final : Statement {
    ReturnStatement(std::unique_ptr<Expression> returnValue, const uint32_t start,
                    const uint32_t end, const FileID fileID)
        : Statement{NodeKind::RETURN_STATEMENT, start, end, fileID},
          returnValue_(std::move(returnValue)) {}

    std::unique_ptr<Expression> returnValue_;
};

struct ExternalFunctionDeclaration final : Node {
    ExternalFunctionDeclaration(std::unique_ptr<Identifier> identifier,
                                std::vector<std::unique_ptr<VariableDefinition>> parameters,
                                const TypeID returnTypeID, const uint32_t start, const uint32_t end,
                                const FileID fileID)
        : Node{NodeKind::EXTERNAL_FUNCTION_DECLARATION, start, end, fileID},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          returnTypeID_(returnTypeID) {}

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<VariableDefinition>> parameters_;
    const TypeID returnTypeID_;
};

struct FunctionDefinition final : Node {
    FunctionDefinition(std::unique_ptr<Identifier> identifier,
                       std::vector<std::unique_ptr<VariableDefinition>> parameters,
                       const TypeID returnTypeID, const bool isExported,
                       std::unique_ptr<BlockStatement> body, const uint32_t start,
                       const uint32_t end, const FileID fileID)
        : Node{NodeKind::FUNCTION_DEFINITION, start, end, fileID},
          identifier_(std::move(identifier)),
          parameters_(std::move(parameters)),
          returnTypeID_(returnTypeID),
          isExported_(isExported),
          body_(std::move(body)) {}

    std::unique_ptr<Identifier> identifier_;
    const std::vector<std::unique_ptr<VariableDefinition>> parameters_;
    const TypeID returnTypeID_;
    const bool isExported_;
    std::unique_ptr<BlockStatement> body_;
};

struct Program final : Node {
    explicit Program(const FileID fileID) : Node{NodeKind::PROGRAM, 0, 0, fileID} {}

    void appendFunction(std::unique_ptr<FunctionDefinition> function) {
        functions_.emplace_back(std::move(function));
        sourceEndIndex_ = functions_.back()->sourceEndIndex();
    }

    void appendExternFunction(std::unique_ptr<ExternalFunctionDeclaration> externFunction) {
        externalFunctions_.emplace_back(std::move(externFunction));
        sourceEndIndex_ = externalFunctions_.back()->sourceEndIndex();
    }

    std::vector<std::unique_ptr<ExternalFunctionDeclaration>> externalFunctions_;
    std::vector<std::unique_ptr<FunctionDefinition>> functions_;
};

Operator tokenKindToOperator(TokenKind tokenKind);

bool isArithmeticOperator(Operator op);
bool isEqualityOperator(Operator op);
bool isRelationalOperator(Operator op);
bool isComparisonOperator(Operator op);
bool isAssignmentOperator(Operator op);

}  // namespace AST
