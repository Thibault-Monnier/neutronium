#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "Operator.hpp"
#include "lex/TokenKind.hpp"
#include "source/FileID.hpp"
#include "type/TypeID.hpp"

namespace AST {

enum class NodeKind : uint8_t {
    NUMBER_LITERAL,
    BOOLEAN_LITERAL,
    ARRAY_LITERAL,
    REPEAT_ARRAY_LITERAL,
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
    COMPILATION_UNIT,
};

struct Node {
    Node(const NodeKind kind, const uint32_t sourceStartIndex, const uint32_t sourceEndIndex,
         const FileID fileID, const uint8_t flags = 0)
        : kind_(kind),
          flags_(flags),
          fileID_(fileID),
          sourceStartIndex_(sourceStartIndex),
          sourceEndIndex_(sourceEndIndex) {}

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

    const NodeKind kind_;

   protected:
    const uint8_t flags_;

    const FileID fileID_;

    uint32_t sourceStartIndex_;
    uint32_t sourceEndIndex_;
};

struct Expression : Node {
    Expression(const NodeKind kind, const uint32_t sourceStartIndex, const uint32_t sourceEndIndex,
               const FileID fileID, const TypeID typeID, const uint8_t flags = 0)
        : Node{kind, sourceStartIndex, sourceEndIndex, fileID, flags}, typeID_(typeID) {}

    const TypeID typeID_;
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
        : Expression{NodeKind::BOOLEAN_LITERAL,  start, end, fileID, typeID,
                     static_cast<uint8_t>(value)} {}

    [[nodiscard]] bool value() const { return flags_; }
};

struct ArrayLiteral final : Expression {
    ArrayLiteral(const std::span<Expression*> elements, const uint32_t start, const uint32_t end,
                 const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::ARRAY_LITERAL, start, end, fileID, typeID}, elements_(elements) {}

    const std::span<Expression*> elements_;
};

struct RepeatArrayLiteral final : Expression {
    RepeatArrayLiteral(const Expression* element, const NumberLiteral* count, const uint32_t start,
                       const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::REPEAT_ARRAY_LITERAL, start, end, fileID, typeID},
          element_(element),
          count_(count) {}

    const Expression* element_;
    const NumberLiteral* count_;
};

struct Identifier final : Expression {
    Identifier(const std::string_view name, const uint32_t start, const uint32_t end,
               const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::IDENTIFIER, start, end, fileID, typeID}, name_(name) {}

    const std::string_view name_;
};

struct ArrayAccess final : Expression {
    ArrayAccess(const Expression* base, const Expression* index, const uint32_t start,
                const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::ARRAY_ACCESS, start, end, fileID, typeID},
          base_(base),
          index_(index) {}

    const Expression* base_;
    const Expression* index_;
};

struct FunctionCall final : Expression {
    FunctionCall(const Identifier* callee, const std::span<Expression*> arguments,
                 const uint32_t start, const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::FUNCTION_CALL, start, end, fileID, typeID},
          callee_(callee),
          arguments_(arguments) {}

    const Identifier* callee_;
    const std::span<Expression*> arguments_;
};

struct UnaryExpression final : Expression {
    UnaryExpression(const Operator op, const Expression* operand, const uint32_t start,
                    const uint32_t end, const FileID fileID, const TypeID typeID)
        : Expression{NodeKind::UNARY_EXPRESSION, start, end, fileID, typeID},
          operator_(op),
          operand_(operand) {}

    const Operator operator_;
    const Expression* operand_;
};

struct BinaryExpression final : Expression {
    BinaryExpression(const Expression* left, const Operator op, const Expression* right,
                     const uint32_t start, const uint32_t end, const FileID fileID,
                     const TypeID typeID)
        : Expression{NodeKind::BINARY_EXPRESSION, start, end, fileID, typeID},
          operator_(op),
          left_(left),
          right_(right) {}

    const Operator operator_;
    const Expression* left_;
    const Expression* right_;
};

struct Statement : Node {
    using Node::Node;
};

struct BlockStatement final : Statement {
    explicit BlockStatement(const std::span<Statement*> body, const uint32_t start,
                            const uint32_t end, const FileID fileID)
        : Statement{NodeKind::BLOCK_STATEMENT, start, end, fileID}, body_(body) {}

    const std::span<Statement*> body_;
};

struct VariableDefinition final : Statement {
    VariableDefinition(const Identifier* identifier, const TypeID typeID, const bool isMutable,
                       const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end, fileID,
                    isMutable ? FlagIsMutable : static_cast<uint8_t>(0)},
          typeID_(typeID),
          identifier_(identifier),
          value_(nullptr) {}

    VariableDefinition(const Identifier* identifier, const TypeID typeID, const bool isMutable,
                       const Expression* value, const uint32_t start, const uint32_t end,
                       const FileID fileID)
        : Statement{NodeKind::VARIABLE_DEFINITION, start, end, fileID,
                    isMutable ? FlagIsMutable : static_cast<uint8_t>(0)},
          typeID_(typeID),
          identifier_(identifier),
          value_(value) {}

    const TypeID typeID_;
    const Identifier* identifier_;
    const Expression* value_;

    [[nodiscard]] bool isMutable() const { return (flags_ & FlagIsMutable) != 0; }

   private:
    static constexpr uint8_t FlagIsMutable = 1 << 0;
};

struct Assignment final : Statement {
    Assignment(const Expression* place, const Operator op, const Expression* value,
               const uint32_t start, const uint32_t end, const FileID fileID)
        : Statement{NodeKind::ASSIGNMENT, start, end, fileID},
          operator_(op),
          place_(place),
          value_(value) {}

    const Operator operator_;
    const Expression* place_;
    const Expression* value_;
};

struct ExpressionStatement final : Statement {
    ExpressionStatement(const Expression* expression, const uint32_t start, const uint32_t end,
                        const FileID fileID)
        : Statement{NodeKind::EXPRESSION_STATEMENT, start, end, fileID}, expression_(expression) {}

    const Expression* expression_;
};

struct IfStatement final : Statement {
    IfStatement(const Expression* condition, const BlockStatement* body,
                const BlockStatement* elseClause, const uint32_t start, const uint32_t end,
                const FileID fileID)
        : Statement{NodeKind::IF_STATEMENT, start, end, fileID},
          condition_(condition),
          body_(body),
          elseClause_(elseClause) {}

    const Expression* condition_;
    const BlockStatement* body_;
    const BlockStatement* elseClause_;
};

struct WhileStatement final : Statement {
    WhileStatement(const Expression* condition, const BlockStatement* body, const uint32_t start,
                   const uint32_t end, const FileID fileID)
        : Statement{NodeKind::WHILE_STATEMENT, start, end, fileID},
          condition_(condition),
          body_(body) {}

    const Expression* condition_;
    const BlockStatement* body_;
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
    ExitStatement(const Expression* exitCode, const uint32_t start, const uint32_t end,
                  const FileID fileID)
        : Statement{NodeKind::EXIT_STATEMENT, start, end, fileID}, exitCode_(exitCode) {}

    const Expression* exitCode_;
};

struct ReturnStatement final : Statement {
    ReturnStatement(const Expression* returnValue, const uint32_t start, const uint32_t end,
                    const FileID fileID)
        : Statement{NodeKind::RETURN_STATEMENT, start, end, fileID}, returnValue_(returnValue) {}

    const Expression* returnValue_;
};

struct ExternalFunctionDeclaration final : Node {
    ExternalFunctionDeclaration(const Identifier* identifier,
                                const std::span<VariableDefinition*> parameters,
                                const TypeID returnTypeID, const uint32_t start, const uint32_t end,
                                const FileID fileID)
        : Node{NodeKind::EXTERNAL_FUNCTION_DECLARATION, start, end, fileID},
          returnTypeID_(returnTypeID),
          identifier_(identifier),
          parameters_(parameters) {}

    const TypeID returnTypeID_;
    const Identifier* identifier_;
    const std::span<VariableDefinition*> parameters_;
};

struct FunctionDefinition final : Node {
    FunctionDefinition(const Identifier* identifier,
                       const std::span<VariableDefinition*> parameters, const TypeID returnTypeID,
                       const bool isExported, const BlockStatement* body, const uint32_t start,
                       const uint32_t end, const FileID fileID)
        : Node{NodeKind::FUNCTION_DEFINITION, start, end, fileID,
               isExported ? FlagIsExported : static_cast<uint8_t>(0)},
          returnTypeID_(returnTypeID),
          identifier_(identifier),
          parameters_(parameters),
          body_(body) {}

    const TypeID returnTypeID_;
    const Identifier* identifier_;
    const std::span<VariableDefinition*> parameters_;
    const BlockStatement* body_;

    [[nodiscard]] bool isExported() const { return (flags_ & FlagIsExported) != 0; }

   private:
    static constexpr uint8_t FlagIsExported = 1 << 0;
};

struct CompilationUnit final : Node {
    explicit CompilationUnit(const std::span<ExternalFunctionDeclaration*> externalFunctions,
                             const std::span<FunctionDefinition*> functions, const FileID fileID,
                             const uint32_t sourceEndIndex)
        : Node{NodeKind::COMPILATION_UNIT, 0, sourceEndIndex, fileID},
          externalFunctions_(externalFunctions),
          functions_(functions) {}

    const std::span<ExternalFunctionDeclaration*> externalFunctions_;
    const std::span<FunctionDefinition*> functions_;
};

Operator tokenKindToOperator(TokenKind tokenKind);

bool isArithmeticOperator(Operator op);
bool isLogicalOperator(Operator op);
bool isEqualityOperator(Operator op);
bool isRelationalOperator(Operator op);
bool isComparisonOperator(Operator op);
bool isAssignmentOperator(Operator op);

}  // namespace AST
