#pragma once

#include <sstream>
#include <string>
#include <unordered_map>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "driver/Cli.hpp"
#include "type/TypeManager.hpp"

namespace CodeGen {

class Generator {
   public:
    explicit Generator(const AST::Program& ast, const TypeManager& typeManager,
                       const TargetType targetType)
        : program_(ast), typeManager_(typeManager), targetType_(targetType) {}

    [[nodiscard]] std::stringstream generate();

   private:
    const AST::Program& program_;
    std::stringstream output_;

    const TypeManager& typeManager_;

    const TargetType targetType_;

    int labelsCount_ = 0;
    std::string innerLoopStartLabel_;
    std::string innerLoopEndLabel_;

    static constexpr int INITIAL_STACK_OFFSET = 0;
    int currentStackOffset_ = INITIAL_STACK_OFFSET;

    static constexpr int FUNCTION_ARGUMENT_SIZE_BITS = 64;

    SymbolTable symbolTable_;

    /**
     * @brief Computes the size in bits of the provided type.
     */
    int typeSizeBits(const Type& type) const { return type.sizeBits(typeManager_); }

    /**
     * @brief Computes the size in bits of the provided expression.
     */
    int exprSizeBits(const AST::Expression& expr) const {
        const Type& type = typeManager_.getType(expr.typeID_);
        return typeSizeBits(type);
    }

    void insertSymbol(const std::string& name, TypeID typeID);
    int getScopeFrameSize(const AST::BlockStatement& blockStmt) const;
    void enterScope(const AST::BlockStatement& blockStmt);
    void exitScope(const AST::BlockStatement& blockStmt);

    int getVariableSizeBits(const std::string& name) const;
    int getVariableStackOffset(const std::string& name) const;
    static std::string_view sizeDirective(int bitSize);
    static std::string_view registerAForSize(int bitSize);

    static std::string label(int labelID);

    /**
     * @brief Cleans the rax register by zero-extending if necessary.
     */
    void cleanRax(int raxValueSizeBits);
    void loadValueFromRax(int bitSize);

    void push(std::string_view reg, int sizeBits = 64);
    void pop(std::string_view reg, int sizeBits = 64);
    void allocateStackSpace(int sizeBits);
    void freeStackSpace(int sizeBits);
    /** @brief Gets a persistent memory operand string representing the current top of the stack.
     *
     * @return A string providing persistent access to the current
     * top of the stack, in the form of "[rbp - offset]". This will not be affected by later rsp
     * changes, so it is safe to use across multiple pushes/allocations.
     */
    std::string stackTopMemoryOperand() const;

    void writeToVariableFromRax(const std::string& name);
    void moveVariableToRax(const std::string& name);
    void moveNumberLitToRax(const AST::NumberLiteral& numberLit);
    void moveBooleanLitToRax(const AST::BooleanLiteral& booleanLit);

    void evaluatePlaceExpressionAddressToRax(const AST::Expression& place);

    void generateArrayLit(const AST::ArrayLiteral& arrayLit, std::string_view destinationAddress);
    void allocateAndGenerateArrayLiteral(const AST::ArrayLiteral& arrayLit);

    static std::string functionNameWithPrefix(const std::string& name);
    void generateFunctionCall(const AST::FunctionCall& funcCall,
                              const std::optional<std::string_view>& destinationAddress);
    void allocateAndGenerateFunctionCall(const AST::FunctionCall& funcCall);
    void evaluateUnaryExpressionToRax(const AST::UnaryExpression& unaryExpr);
    void applyArithmeticOperatorToRax(AST::Operator op, const std::string& other);
    void evaluateBinaryExpressionToRax(const AST::BinaryExpression& binaryExpr);
    void generatePrimitiveExpression(const AST::Expression& expr,
                                     const std::optional<std::string_view>& destinationAddress);
    void generateArrayExpression(const AST::Expression& expr,
                                 const std::optional<std::string_view>& destinationAddress);

    void evaluateExpressionToRax(const AST::Expression& expr) {
        generateExpression(expr, std::nullopt);
    }
    void generateExpression(const AST::Expression& expr,
                            const std::optional<std::string_view>& destinationAddress);
    void copyArrayContents(std::string_view sourceAddress, std::string_view destinationAddress,
                           int arraySizeBits);

    int generateCondition(const AST::Expression& condition);
    void generateVariableDefinition(const AST::VariableDefinition& varDecl);
    void generateVariableAssignment(const AST::Assignment& assignment);
    void generateExpressionStmt(const AST::ExpressionStatement& exprStmt);
    void generateIfStmt(const AST::IfStatement& ifStmt);
    void generateWhileStmt(const AST::WhileStatement& whileStmt);
    void generateBreakStatement();
    void generateContinueStatement();
    void generateReturnStatement(const AST::ReturnStatement& returnStmt);
    void generateExit(const std::string& source);
    void generateExit(const AST::ExitStatement& exitStmt);

    void generateStmt(const AST::Statement& stmt);
    void generateFunctionDefinition(const AST::FunctionDefinition& funcDef);
};

}  // namespace CodeGen