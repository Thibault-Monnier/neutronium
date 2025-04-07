#include "parsing/AST.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "lexing/token_kind.hpp"

namespace AST {

Operator token_kind_to_AST_operator(const TokenKind tokenKind) {
    switch (tokenKind) {
        case TokenKind::PLUS:
            return Operator::ADD;
        case TokenKind::MINUS:
            return Operator::SUBTRACT;
        case TokenKind::STAR:
            return Operator::MULTIPLY;
        case TokenKind::SLASH:
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
            if constexpr (std::is_same_v<std::decay_t<T0>, PrimaryExpression>) {
                std::visit(
                    [&](const auto& inner) {
                        using InnerT = std::decay_t<decltype(inner)>;
                        if constexpr (std::is_same_v<InnerT, NumberLiteral>) {
                            std::cout << prefix << branch << "NumberLiteral: " << inner.value_
                                      << "\n";
                        } else if constexpr (std::is_same_v<InnerT, Identifier>) {
                            std::cout << prefix << branch << "Identifier: " << inner.name_ << "\n";
                        }
                    },
                    exprPtr);
            } else if constexpr (std::is_same_v<std::decay_t<T0>,
                                                std::shared_ptr<BinaryExpression>>) {
                std::cout << prefix << branch << "BinaryExpression\n";

                const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

                log_expression(exprPtr->left_, newPrefix, false);
                std::cout << newPrefix << "├── Operator: " << operator_to_string(exprPtr->operator_)
                          << "\n";
                log_expression(exprPtr->right_, newPrefix, true);
            } else if constexpr (std::is_same_v<std::decay_t<T0>,
                                          std::shared_ptr<UnaryExpression>>) {
                std::cout << prefix << branch << "UnaryExpression\n";

                const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

                std::cout << newPrefix << "├── Operator: " << operator_to_string(exprPtr->operator_)
                          << "\n";
                log_expression(exprPtr->operand_, newPrefix, true);
            }
        },
        expr);
}

void log_node(const std::shared_ptr<ASTNode>& node, const std::string& prefix, bool isLast) {
    if (node->kind_ == NodeKind::PROGRAM) {
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
                    } else if constexpr (std::is_same_v<T, Exit>) {
                        std::cout << stmtPrefix << "└── Exit\n";
                        std::cout << stmtPrefix << "    └── ExitCode\n";
                        log_expression(s.exitCode_, stmtPrefix + "        ", true);
                    }
                },
                stmt);
        }
    }
}

}  // namespace AST