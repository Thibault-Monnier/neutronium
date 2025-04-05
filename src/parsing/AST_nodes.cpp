#include "parsing/AST_nodes.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace AST {

void log_expression(const Expression& expr, int indent) {
    std::visit(
        [indent](const auto& exprPtr) {
            if constexpr (std::is_same_v<std::decay_t<decltype(exprPtr)>,
                                         std::shared_ptr<PrimaryExpression>>) {
                std::cout << std::string(indent, ' ') << "PrimaryExpression: " << exprPtr->value_
                          << "\n";
            } else if constexpr (std::is_same_v<std::decay_t<decltype(exprPtr)>,
                                                std::shared_ptr<BinaryExpression>>) {
                std::cout << std::string(indent, ' ') << "BinaryExpression:\n";
                std::cout << std::string(indent + 2, ' ') << "Left:\n";
                log_expression(exprPtr->left_, indent + 4);
                std::cout << std::string(indent + 2, ' ')
                          << "Operator: " << static_cast<int>(exprPtr->operator_) << "\n";
                std::cout << std::string(indent + 2, ' ') << "Right:\n";
                log_expression(exprPtr->right_, indent + 4);
            }
        },
        expr);
}

void log_node(const std::shared_ptr<ASTNode>& node, int indent) {
    if (node->type_ == PROGRAM) {
        auto program = std::dynamic_pointer_cast<Program>(node);
        if (!program) return;
        std::cout << std::string(indent, ' ') << "Program:\n";
        for (const auto& stmt : program->statements_) {
            std::cout << std::string(indent + 2, ' ') << "Statement:\n";
            std::visit(
                [indent](const auto& s) {
                    using T = std::decay_t<decltype(s)>;
                    if constexpr (std::is_same_v<T, Assignment>) {
                        std::cout << std::string(indent + 4, ' ') << "Assignment:\n";
                        std::cout << std::string(indent + 6, ' ') << "Identifier: " << s.identifier_
                                  << "\n";
                        std::cout << std::string(indent + 6, ' ') << "Value:\n";
                        log_expression(s.value_, indent + 8);
                    } else if constexpr (std::is_same_v<T, Expression>) {
                        std::cout << std::string(indent + 4, ' ') << "Expression:\n";
                        log_expression(s, indent + 6);
                    }
                },
                stmt);
        }
    }
}

}  // namespace AST