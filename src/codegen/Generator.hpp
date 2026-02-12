#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "driver/Cli.hpp"
#include "lib/FastStringStream.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

namespace CodeGen {

/**
 * @brief Responsible for generating code based on the abstract syntax tree (AST) and other
 * contextual information, like type management and target architecture.
 */
class Generator {
   public:
    explicit Generator(const AST::CompilationUnit& ast, const TypeManager& typeManager,
                       const TargetType targetType)
        : compilationUnit_(ast), typeManager_(typeManager), targetType_(targetType) {}

    [[nodiscard]] neutro::FastStringStream generate();

   private:
    const AST::CompilationUnit& compilationUnit_;
    neutro::FastStringStream output_;

    const TypeManager& typeManager_;

    const TargetType targetType_;

    int labelsCount_ = 0;
    std::string innerLoopStartLabel_;
    std::string innerLoopEndLabel_;

    uint32_t currentSpillStackOffset_ = 0;
    uint32_t currentSymbolsStackOffset_ = 0;

    static constexpr uint32_t FUNCTION_ARGUMENT_SIZE_BITS = 64;

    SymbolTable symbolTable_;

    /**
     * @brief Computes the size of the provided type in bits.
     */
    [[nodiscard]] uint32_t typeSizeBits(const Type& type) const {
        return type.sizeBits(typeManager_);
    }

    /**
     * @brief Computes the size of the provided expression in bits.
     */
    [[nodiscard]] uint32_t exprSizeBits(const AST::Expression& expr) const {
        const Type& type = typeManager_.getType(expr.typeID_);
        return typeSizeBits(type);
    }

    /**
     * @brief Computes the size of the provided variable definition in bits.
     */
    [[nodiscard]] uint32_t varDefSizeBits(const AST::VariableDefinition& varDef) const {
        const Type& type = typeManager_.getType(varDef.typeID_);
        return typeSizeBits(type);
    }

    void insertSymbol(std::string_view name, TypeID typeID);
    [[nodiscard]] uint32_t getScopeFrameSize(const AST::BlockStatement& blockStmt) const;
    void enterScope(const AST::BlockStatement& blockStmt);

    [[nodiscard]] uint32_t getVariableSizeBits(std::string_view name) const;
    [[nodiscard]] uint32_t getVariableStackOffset(std::string_view name) const;

    /**
     * @brief Gets a persistent memory operand string for a specific stack offset.
     *
     * @param offset The offset from the base pointer (rbp) in bits.
     * @return A string representing a memory operand in the form of "[rbp - offset]" that
     * provides persistent access to the stack location at the given offset. This will not be
     * affected by later rsp changes, so it is safe to use across multiple pushes/allocations.
     */
    [[nodiscard]] static std::string stackOffsetMemoryOperand(uint32_t offset);
    /**
     * @brief Gets a persistent memory operand string representing the current top of the stack.
     *
     * @return A string providing persistent access to the current
     * top of the stack, in the form of "[rbp - offset]". This will not be affected by later rsp
     * changes, so it is safe to use across multiple pushes/allocations.
     */
    [[nodiscard]] std::string stackTopMemoryOperand() const;
    [[nodiscard]] std::string getVariableStackMemoryOperand(std::string_view name) const;

    static std::string_view registerAForSize(uint32_t bitSize);

    static std::string label(uint32_t labelID);

    /**
     * @brief Cleans the rax register by zero-extending if necessary.
     */
    void cleanRax(uint32_t raxValueSizeBits);
    void loadValueFromRax(uint32_t bitSize);

    /**
     * @brief Pushes the provided register onto the stack and returns the stack offset in bits where
     * it was pushed.
     */
    uint32_t push(std::string_view reg, uint32_t sizeBits = 64);
    /**
     * @brief Pops a value from the stack at the provided stack offset in bits into the provided
     * register. The offset should usually be the one returned by a previous call to push.
     */
    void pop(std::string_view reg, uint32_t offsetBits);

    void allocateStackSpace(uint32_t sizeBits);
    void setStackOffset(uint32_t offsetBits);
    void updateRsp();

    /** @brief Copies the value in rax to the provided destination operand, which should be a memory
     * operand, according to the type of the value.
     */
    void copyTo(TypeID typeID, std::string_view to);

    void writeToVariableFromRax(std::string_view name);
    void moveVariableToRax(std::string_view name);
    void moveNumberLitToRax(const AST::NumberLiteral& numberLit);
    void moveBooleanLitToRax(const AST::BooleanLiteral& booleanLit);

    void evaluatePlaceExpressionAddressToRax(const AST::Expression& place);

    void generateArrayLit(const AST::ArrayLiteral& arrayLit, uint32_t destinationStackOffset);
    void allocateAndGenerateArrayLiteral(const AST::ArrayLiteral& arrayLit);

    static std::string functionNameWithPrefix(std::string_view name);
    void generateFunctionCall(const AST::FunctionCall& funcCall,
                              const std::optional<uint32_t>& destinationStackOffset);
    void allocateAndGenerateFunctionCall(const AST::FunctionCall& funcCall);
    void evaluateUnaryExpressionToRax(const AST::UnaryExpression& unaryExpr);
    void applyArithmeticOperatorToRax(AST::Operator op, const std::string& other);
    void evaluateBinaryExpressionToRax(const AST::BinaryExpression& binaryExpr);
    void generatePrimitiveExpression(const AST::Expression& expr,
                                     const std::optional<uint32_t>& destinationStackOffset);
    void generateArrayExpression(const AST::Expression& expr,
                                 const std::optional<uint32_t>& destinationStackOffset);

    void evaluateExpressionToRax(const AST::Expression& expr);
    void generateExpression(const AST::Expression& expr,
                            const std::optional<uint32_t>& destinationStackOffset);
    void copyArrayContents(std::string_view sourceAddress, std::string_view destinationAddress,
                           uint32_t arraySizeBits);
    void copyArrayContents(std::string_view sourceAddress, uint32_t destinationStackOffset,
                           uint32_t arraySizeBits);

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