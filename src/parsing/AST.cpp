#include "parsing/AST.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lexing/token_type.hpp"

namespace AST {

Operator token_type_to_AST_operator(const TokenType tokenType) {
    switch (tokenType) {
        case TokenType::PLUS:
            return Operator::ADD;
        case TokenType::MINUS:
            return Operator::SUBTRACT;
        case TokenType::STAR:
            return Operator::MULTIPLY;
        case TokenType::SLASH:
            return Operator::DIVIDE;
        default:
            return Operator::UNDEFINED_OPERATOR;
    }
}

std::string operator_to_string(const Operator op) {
    switch (op) {
        case Operator::ADD:
            return "+";
        case Operator::SUBTRACT:
            return "-";
        case Operator::MULTIPLY:
            return "*";
        case Operator::DIVIDE:
            return "/";
        default:
            throw std::invalid_argument("Invalid operator passed to AST::operator_to_string");
    }
}

void log_expression(const Expression& expr, const std::string& prefix, bool isLast) {
    std::string branch = isLast ? "└── " : "├── ";

    std::visit(
        [&]<typename T0>(const T0& exprPtr) {
            if constexpr (std::is_same_v<std::decay_t<T0>,
                                         std::shared_ptr<PrimaryExpression>>) {
                std::cout << prefix << branch << "PrimaryExpression: " << exprPtr->value_ << "\n";
            } else if constexpr (std::is_same_v<std::decay_t<T0>,
                                                std::shared_ptr<BinaryExpression>>) {
                std::cout << prefix << branch << "BinaryExpression\n";

                const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

                log_expression(exprPtr->left_, newPrefix, false);
                std::cout << newPrefix << "├── Operator: " << operator_to_string(exprPtr->operator_)
                          << "\n";
                log_expression(exprPtr->right_, newPrefix, true);
            }
        },
        expr);
}

void log_node(const std::shared_ptr<ASTNode>& node, const std::string& prefix, bool isLast) {
    if (node->type_ == PROGRAM) {
        auto program = std::dynamic_pointer_cast<Program>(node);
        if (!program) return;

        std::cout << "Program\n";
        const std::string newPrefix = prefix + (isLast ? "" : "│");

        for (size_t i = 0; i < program->statements_.size(); ++i) {
            const auto& stmt = program->statements_[i];
            const bool stmtIsLast = (i == program->statements_.size() - 1);

            std::cout << newPrefix << (stmtIsLast ? "└── " : "├── ") << "Statement\n";
            std::string stmtPrefix = newPrefix + (stmtIsLast ? "    " : "│   ");

            std::visit(
                [&](const auto& s) {
                    using T = std::decay_t<decltype(s)>;
                    if constexpr (std::is_same_v<T, Assignment>) {
                        std::cout << stmtPrefix << "├── Assignment\n";
                        std::cout << stmtPrefix << "│   ├── Identifier: " << s.identifier_ << "\n";
                        std::cout << stmtPrefix << "│   └── Value\n";
                        log_expression(s.value_, stmtPrefix + "│       ", true);
                    } else if constexpr (std::is_same_v<T, Expression>) {
                        std::cout << stmtPrefix << "└── Expression\n";
                        log_expression(s, stmtPrefix + "    ", true);
                    }
                },
                stmt);
        }
    }
}

}  // namespace AST