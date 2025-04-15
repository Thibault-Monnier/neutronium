#include "parsing/AST.hpp"

#include <cmath>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "lexing/token_kind.hpp"

namespace AST {

std::string node_kind_to_string(const NodeKind kind) {
    switch (kind) {
        case NodeKind::NUMBER_LITERAL:
            return "NUMBER_LITERAL";
        case NodeKind::IDENTIFIER:
            return "IDENTIFIER";
        case NodeKind::UNARY_EXPRESSION:
            return "UNARY_EXPRESSION";
        case NodeKind::BINARY_EXPRESSION:
            return "BINARY_EXPRESSION";
        case NodeKind::ASSIGNMENT:
            return "ASSIGNMENT";
        case NodeKind::EXIT:
            return "EXIT";
        case NodeKind::PROGRAM:
            return "PROGRAM";
        default:
            throw std::invalid_argument("Invalid NodeKind passed to AST::node_kind_to_string");
    }
}

Operator token_kind_to_operator(const TokenKind tokenKind) {
    switch (tokenKind) {
        case TokenKind::PLUS:
            return Operator::ADD;
        case TokenKind::MINUS:
            return Operator::SUBTRACT;
        case TokenKind::STAR:
            return Operator::MULTIPLY;
        case TokenKind::SLASH:
            return Operator::DIVIDE;
        case TokenKind::EQUAL_EQUAL:
            return Operator::EQUALS;
        case TokenKind::NOT_EQUAL:
            return Operator::NOT_EQUALS;
        case TokenKind::LESS_THAN:
            return Operator::LESS_THAN;
        case TokenKind::LESS_THAN_EQUAL:
            return Operator::LESS_THAN_OR_EQUAL;
        case TokenKind::GREATER_THAN:
            return Operator::GREATER_THAN;
        case TokenKind::GREATER_THAN_EQUAL:
            return Operator::GREATER_THAN_OR_EQUAL;
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
        case Operator::EQUALS:
            return "==";
        case Operator::NOT_EQUALS:
            return "!=";
        case Operator::LESS_THAN:
            return "<";
        case Operator::LESS_THAN_OR_EQUAL:
            return "<=";
        case Operator::GREATER_THAN:
            return ">";
        case Operator::GREATER_THAN_OR_EQUAL:
            return ">=";
        default:
            throw std::invalid_argument("Invalid operator passed to AST::operator_to_string");
    }
}

bool is_arithmetic_operator(const Operator op) {
    return op == Operator::ADD || op == Operator::SUBTRACT || op == Operator::MULTIPLY ||
           op == Operator::DIVIDE;
}

bool is_equality_operator(const Operator op) {
    return op == Operator::EQUALS || op == Operator::NOT_EQUALS;
}

bool is_relational_operator(const Operator op) {
    return op == Operator::LESS_THAN || op == Operator::LESS_THAN_OR_EQUAL ||
           op == Operator::GREATER_THAN || op == Operator::GREATER_THAN_OR_EQUAL;
}

bool is_comparison_operator(const Operator op) {
    return is_equality_operator(op) || is_relational_operator(op);
}

void log_expression(const Expression& expr, const std::string& prefix, const bool isLast) {
    const std::string branch = isLast ? "└── " : "├── ";

    if (expr.kind_ == NodeKind::NUMBER_LITERAL) {
        const NumberLiteral numberLit = static_cast<const NumberLiteral&>(expr);
        std::cout << prefix << branch << "NumberLiteral: " << numberLit.value_ << "\n";
    } else if (expr.kind_ == NodeKind::BOOLEAN_LITERAL) {
        const BooleanLiteral booleanLit = static_cast<const BooleanLiteral&>(expr);
        std::cout << prefix << branch
                  << "BooleanLiteral: " << (booleanLit.value_ ? "true" : "false") << "\n";
    } else if (expr.kind_ == NodeKind::IDENTIFIER) {
        const Identifier identifier = static_cast<const Identifier&>(expr);
        std::cout << prefix << branch << "Identifier: " << identifier.name_ << "\n";
    } else {
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

        if (expr.kind_ == NodeKind::BINARY_EXPRESSION) {
            const auto& binaryExpr = static_cast<const BinaryExpression&>(expr);
            std::cout << prefix << branch << "BinaryExpression\n";
            log_expression(*binaryExpr.left_, newPrefix, false);
            std::cout << newPrefix << "├── Operator: " << operator_to_string(binaryExpr.operator_)
                      << "\n";
            log_expression(*binaryExpr.right_, newPrefix, true);
        } else if (expr.kind_ == NodeKind::UNARY_EXPRESSION) {
            const auto& unaryExpr = static_cast<const UnaryExpression&>(expr);
            std::cout << prefix << branch << "UnaryExpression\n";
            std::cout << newPrefix << "├── Operator: " << operator_to_string(unaryExpr.operator_)
                      << "\n";
            log_expression(*unaryExpr.operand_, newPrefix, true);
        }
    }
}

void log_statement(const Statement& stmt, const std::string& prefix, const bool isLast) {
    std::cout << prefix << (isLast ? "└── " : "├── ") << "Statement\n";
    const std::string stmtPrefix = prefix + (isLast ? "    " : "│   ");

    if (stmt.kind_ == NodeKind::ASSIGNMENT) {
        const auto& assignment = *static_cast<const Assignment*>(&stmt);
        if (assignment.isDeclaration_) {
            std::cout << stmtPrefix << "├── Declaration Assignment\n";
        } else {
            std::cout << stmtPrefix << "├── Assignment\n";
        }
        std::cout << stmtPrefix << "│   ├── Identifier: " << assignment.identifier_->name_ << "\n";
        std::cout << stmtPrefix << "│   └── Value\n";
        log_expression(*assignment.value_, stmtPrefix + "│       ", true);
    } else if (stmt.kind_ == NodeKind::IF_STATEMENT) {
        const auto& ifStmt = *static_cast<const IfStatement*>(&stmt);
        std::cout << stmtPrefix << "├── IfStatement\n";
        std::cout << stmtPrefix << "│   ├── Condition\n";
        log_expression(*ifStmt.condition_, stmtPrefix + "│   │   ", true);
        std::cout << stmtPrefix << "│   └── Body\n";
        log_statement(*ifStmt.body_, stmtPrefix + "│       ", true);
    } else if (stmt.kind_ == NodeKind::EXIT) {
        const auto& exit = *static_cast<const Exit*>(&stmt);
        std::cout << stmtPrefix << "└── Exit\n";
        std::cout << stmtPrefix << "    └── ExitCode\n";
        log_expression(*exit.exitCode_, stmtPrefix + "        ", true);
    }
}

void log_ast(const Program& programNode) {
    std::cout << "Program\n";

    for (size_t i = 0; i < programNode.statements_.size(); ++i) {
        const auto& stmt = programNode.statements_[i];
        const bool isLast = (i == programNode.statements_.size() - 1);
        log_statement(*stmt, "", isLast);
    }
}

}  // namespace AST