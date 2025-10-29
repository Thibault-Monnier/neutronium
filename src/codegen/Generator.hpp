#pragma once

#include <sstream>
#include <string>
#include <unordered_map>

#include "../Cli.hpp"
#include "../ast/AST.hpp"
#include "../type/TypeManager.hpp"
#include "SymbolTable.hpp"

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

    void insert_symbol(const std::string& name, TypeID typeID);
    int get_scope_frame_size(const AST::BlockStatement& blockStmt) const;
    void enter_scope(const AST::BlockStatement& blockStmt);
    void exit_scope(const AST::BlockStatement& blockStmt);

    int get_variable_size_bits(const std::string& name) const;
    int get_variable_stack_offset(const std::string& name) const;
    static std::string_view size_directive(int bitSize);
    static std::string_view register_a_for_size(int bitSize);

    static std::string label(int labelID);

    /**
     * @brief Cleans the rax register by zero-extending if necessary.
     */
    void clean_rax(int raxValueSizeBits);
    void load_value_from_rax(int bitSize);

    void push(std::string_view reg, int sizeBits = 64);
    void pop(std::string_view reg, int sizeBits = 64);
    void allocate_stack_space(int sizeBits);
    void free_stack_space(int sizeBits);
    /** @brief Gets a persistent memory operand string representing the current top of the stack.
     *
     * Returns a string providing persistent access to the current
     * top of the stack, in the form of "[rbp - offset]". This will not be affected by later rsp
     * changes, so it is safe to use across multiple pushes/allocations.
     */
    std::string stack_top_memory_operand() const;

    void write_to_variable_from_rax(const std::string& name);
    void move_variable_to_rax(const std::string& name);
    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit);
    void move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit);

    void evaluate_place_expression_address_to_rax(const AST::Expression& place);

    void generate_array_lit(const AST::ArrayLiteral& arrayLit, std::string_view destinationAddress);
    void allocate_and_generate_array_literal(const AST::ArrayLiteral& arrayLit);

    static std::string function_name_with_prefix(const std::string& name);
    void generate_function_call(const AST::FunctionCall& funcCall,
                                const std::optional<std::string_view>& destinationAddress);
    void allocate_and_generate_function_call(const AST::FunctionCall& funcCall);
    void evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr);
    void apply_arithmetic_operator_to_rax(AST::Operator op, const std::string& other);
    void evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr);
    void generate_primitive_expression(const AST::Expression& expr,
                                       const std::optional<std::string_view>& destinationAddress);
    void generate_array_expression(const AST::Expression& expr,
                                   const std::optional<std::string_view>& destinationAddress);

    void evaluate_expression_to_rax(const AST::Expression& expr) {
        generate_expression(expr, std::nullopt);
    }
    void generate_expression(const AST::Expression& expr,
                             const std::optional<std::string_view>& destinationAddress);
    void copy_array_contents(std::string_view sourceAddress, std::string_view destinationAddress,
                             int arraySizeBits);

    int generate_condition(const AST::Expression& condition);
    void generate_variable_definition(const AST::VariableDefinition& varDecl);
    void generate_variable_assignment(const AST::Assignment& assignment);
    void generate_expression_stmt(const AST::ExpressionStatement& exprStmt);
    void generate_if_stmt(const AST::IfStatement& ifStmt);
    void generate_while_stmt(const AST::WhileStatement& whileStmt);
    void generate_break_statement();
    void generate_continue_statement();
    void generate_return_statement(const AST::ReturnStatement& returnStmt);
    void generate_exit(const std::string& source);
    void generate_exit(const AST::ExitStatement& exitStmt);

    void generate_stmt(const AST::Statement& stmt);
    void generate_function_definition(const AST::FunctionDefinition& funcDef);
};

}  // namespace CodeGen