#include "parsing/AST.hpp"

#include <cmath>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include "lexing/token_kind.hpp"

namespace AST {

Operator token_kind_to_operator(const TokenKind tokenKind) {
    const std::unordered_map<TokenKind, Operator> tokenToOperatorMap = {
        {TokenKind::PLUS, Operator::ADD},
        {TokenKind::MINUS, Operator::SUBTRACT},
        {TokenKind::STAR, Operator::MULTIPLY},
        {TokenKind::SLASH, Operator::DIVIDE},
        {TokenKind::BANG, Operator::LOGICAL_NOT},
        {TokenKind::EQUAL_EQUAL, Operator::EQUALS},
        {TokenKind::BANG_EQUAL, Operator::NOT_EQUALS},
        {TokenKind::LESS_THAN, Operator::LESS_THAN},
        {TokenKind::LESS_THAN_EQUAL, Operator::LESS_THAN_OR_EQUAL},
        {TokenKind::GREATER_THAN, Operator::GREATER_THAN},
        {TokenKind::GREATER_THAN_EQUAL, Operator::GREATER_THAN_OR_EQUAL},
    };

    auto it = tokenToOperatorMap.find(tokenKind);
    return it != tokenToOperatorMap.end() ? it->second : Operator::UNDEFINED_OPERATOR;
}

std::string operator_to_string(const Operator op) {
    static const std::unordered_map<Operator, std::string> table = {
        {Operator::ADD, "+"},
        {Operator::SUBTRACT, "-"},
        {Operator::MULTIPLY, "*"},
        {Operator::DIVIDE, "/"},
        {Operator::LOGICAL_NOT, "!"},
        {Operator::EQUALS, "=="},
        {Operator::NOT_EQUALS, "!="},
        {Operator::LESS_THAN, "<"},
        {Operator::LESS_THAN_OR_EQUAL, "<="},
        {Operator::GREATER_THAN, ">"},
        {Operator::GREATER_THAN_OR_EQUAL, ">="},
    };

    auto it = table.find(op);
    if (it != table.end()) return it->second;
    throw std::invalid_argument("Invalid operator");
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

void log_statement(const Statement& stmt, const std::string& prefix) {
    if (stmt.kind_ == NodeKind::ASSIGNMENT) {
        const auto& assignment = *static_cast<const Assignment*>(&stmt);
        if (assignment.isDeclaration_) {
            std::cout << prefix << "├── Declaration Assignment\n";
        } else {
            std::cout << prefix << "├── Assignment\n";
        }
        std::cout << prefix << "│   ├── Identifier: " << assignment.identifier_->name_ << "\n";
        std::cout << prefix << "│   └── Value\n";
        log_expression(*assignment.value_, prefix + "│       ", true);
    } else if (stmt.kind_ == NodeKind::IF_STATEMENT) {
        const auto& ifStmt = *static_cast<const IfStatement*>(&stmt);
        std::cout << prefix << "├── IfStatement\n";
        std::cout << prefix << "│   ├── Condition\n";
        log_expression(*ifStmt.condition_, prefix + "│   │   ", true);
        std::cout << prefix << "│   └── Body\n";
        log_statement(*ifStmt.body_, prefix + "│       ");
    } else if (stmt.kind_ == NodeKind::WHILE_STATEMENT) {
        const auto& whileStmt = *static_cast<const WhileStatement*>(&stmt);
        std::cout << prefix << "├── WhileStatement\n";
        std::cout << prefix << "│   ├── Condition\n";
        log_expression(*whileStmt.condition_, prefix + "│   │   ", true);
        std::cout << prefix << "│   └── Body\n";
        log_statement(*whileStmt.body_, prefix + "│       ");
    } else if (stmt.kind_ == NodeKind::FUNCTION_DECLARATION) {
        const auto& funcDecl = *static_cast<const FunctionDeclaration*>(&stmt);
        std::cout << prefix << "├── FunctionDeclaration\n";
        std::cout << prefix << "│   ├── Identifier: " << funcDecl.identifier_->name_ << "\n";
        std::cout << prefix << "│   └── Body\n";
        log_statement(*funcDecl.body_, prefix + "│       ");
    } else if (stmt.kind_ == NodeKind::EXIT) {
        const auto& exit = *static_cast<const Exit*>(&stmt);
        std::cout << prefix << "└── Exit\n";
        std::cout << prefix << "    └── ExitCode\n";
        log_expression(*exit.exitCode_, prefix + "        ", true);
    } else if (stmt.kind_ == NodeKind::BLOCK_STATEMENT) {
        const auto& blockStmt = *static_cast<const BlockStatement*>(&stmt);
        std::cout << prefix << "├── BlockStatement\n";
        for (size_t i = 0; i < blockStmt.body_.size(); ++i) {
            const auto& innerStmt = blockStmt.body_[i];
            log_statement(*innerStmt, prefix + "│   ");
        }
    } else {
        throw std::invalid_argument("Invalid statement kind");
    }
}

void log_ast(const Program& programNode) {
    std::cout << "Program\n";
    const auto& stmt = programNode.body_;
    log_statement(*stmt, "");
}

}  // namespace AST