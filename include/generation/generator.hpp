#pragma once

#include <sstream>
#include <string>

#include "cli.hpp"
#include "parsing/ast.hpp"
#include "semantic-analysis/symbol_table.hpp"
#include "semantic-analysis/types/TypeManager.hpp"

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
    int innerLoopStartLabel_ = 0;
    int innerLoopEndLabel_ = 0;

    static constexpr int INITIAL_STACK_OFFSET = 8;
    int currentStackOffset_ = INITIAL_STACK_OFFSET;
    std::unordered_map<std::string, int> variablesStackOffset_;

    static int get_current_scope_frame_size(const AST::BlockStatement& blockStmt);

    void stack_allocate_scope_variables(const AST::BlockStatement& blockStmt);
    void stack_deallocate_scope_variables(const AST::BlockStatement& blockStmt);
    int get_variable_stack_offset(const std::string& name) const;
    void insert_variable_stack_offset(const std::string& name);

    void write_to_variable(const std::string& name, const std::string& source);
    void move_variable_to_rax(const std::string& name);
    void move_number_lit_to_rax(const AST::NumberLiteral& numberLit);
    void move_boolean_lit_to_rax(const AST::BooleanLiteral& booleanLit);

    void evaluate_array_access_address_to_rax(const AST::ArrayAccess& arrayAccess);
    void evaluate_place_expression_address_to_rax(const AST::Expression& place);

    void write_array_to_heap(const AST::ArrayLiteral& arrayLit);
    void evaluate_array_access_to_rax(const AST::ArrayAccess& arrayAccess);
    static std::string function_name_with_prefix(const std::string& name);
    void generate_function_call(const AST::FunctionCall& funcCall);
    void evaluate_unary_expression_to_rax(const AST::UnaryExpression& unaryExpr);
    void apply_arithmetic_operator_to_rax(AST::Operator op, const std::string& other);
    void evaluate_binary_expression_to_rax(const AST::BinaryExpression& binaryExpr);
    void evaluate_expression_to_rax(const AST::Expression& expr);

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
