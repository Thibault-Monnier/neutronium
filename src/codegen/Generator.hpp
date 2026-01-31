#pragma once

#include <cstdint>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>

#include "SymbolTable.hpp"
#include "ast/AST.hpp"
#include "ast/Operator.hpp"
#include "driver/Cli.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

namespace CodeGen {

class Generator {
   public:
    explicit Generator(const AST::CompilationUnit& ast, const TypeManager& typeManager,
                       const TargetType targetType)
        : compilationUnit_(ast), typeManager_(typeManager), targetType_(targetType) {}

    [[nodiscard]] std::stringstream generate();

   private:
    const AST::CompilationUnit& compilationUnit_;
    std::stringstream output_;

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
    uint32_t typeSizeBits(const Type& type) const { return type.sizeBits(typeManager_); }

    /**
     * @brief Computes the size of the provided expression in bits.
     */
    uint32_t exprSizeBits(const AST::Expression& expr) const {
        const Type& type = typeManager_.getType(expr.typeID_);
        return typeSizeBits(type);
    }

    /**
     * @brief Computes the size of the provided variable definition in bits.
     */
    uint32_t varDefSizeBits(const AST::VariableDefinition& varDef) const {
        const Type& type = typeManager_.getType(varDef.typeID_);
        return typeSizeBits(type);
    }

    void insertSymbol(std::string_view name, TypeID typeID);
    uint32_t getScopeFrameSize(const AST::BlockStatement& blockStmt) const;
    void enterScope(const AST::BlockStatement& blockStmt);

    uint32_t getVariableSizeBits(std::string_view name) const;
    uint32_t getVariableStackOffset(std::string_view name) const;
    static std::string_view registerAForSize(uint32_t bitSize);

    static std::string label(uint32_t labelID);

    /**
     * @brief Cleans the rax register by zero-extending if necessary.
     */
    void cleanRax(uint32_t raxValueSizeBits);
    void loadValueFromRax(uint32_t bitSize);

    uint32_t push(std::string_view reg, uint32_t sizeBits = 64);
    void pop(std::string_view reg, uint32_t offsetBits);
    void allocateStackSpace(uint32_t sizeBits);
    void setStackOffset(uint32_t offsetBits);
    void updateRsp();
    /** @brief Gets a persistent memory operand string representing the current top of the stack.
     *
     * @return A string providing persistent access to the current
     * top of the stack, in the form of "[rbp - offset]". This will not be affected by later rsp
     * changes, so it is safe to use across multiple pushes/allocations.
     */
    std::string stackTopMemoryOperand() const;

    void writeToVariableFromRax(std::string_view name);
    void moveVariableToRax(std::string_view name);
    void moveNumberLitToRax(const AST::NumberLiteral& numberLit);
    void moveBooleanLitToRax(const AST::BooleanLiteral& booleanLit);

    void evaluatePlaceExpressionAddressToRax(const AST::Expression& place);

    void generateArrayLit(const AST::ArrayLiteral& arrayLit, std::string_view destinationAddress);
    void allocateAndGenerateArrayLiteral(const AST::ArrayLiteral& arrayLit);

    static std::string functionNameWithPrefix(std::string_view name);
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

    void evaluateExpressionToRax(const AST::Expression& expr);
    void generateExpression(const AST::Expression& expr,
                            const std::optional<std::string_view>& destinationAddress);
    void copyArrayContents(std::string_view sourceAddress, std::string_view destinationAddress,
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