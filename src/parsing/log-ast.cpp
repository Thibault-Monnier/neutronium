#include <iostream>

#include "parsing/ast.hpp"

namespace AST {

namespace {

std::string next_prefix(const std::string& p, const bool isLast) {
    return p + (isLast ? "    " : "│   ");
}

template <class To, class From>
const To& as(const From& n) {
    return static_cast<const To&>(n);
}

void log_expression(const Expression& expr, const std::string& prefix, const bool isLast) {
    const std::string branch = isLast ? "└── " : "├── ";

    if (expr.kind_ == NodeKind::NUMBER_LITERAL) {
        const auto& numberLit = as<NumberLiteral>(expr);
        std::cout << prefix << branch << "NumberLiteral: " << numberLit.value_ << "\n";
    } else if (expr.kind_ == NodeKind::BOOLEAN_LITERAL) {
        const auto& booleanLit = as<BooleanLiteral>(expr);
        std::cout << prefix << branch
                  << "BooleanLiteral: " << (booleanLit.value_ ? "true" : "false") << "\n";
    } else if (expr.kind_ == NodeKind::IDENTIFIER) {
        const auto& identifier = as<Identifier>(expr);
        std::cout << prefix << branch << "Identifier: " << identifier.name_ << "\n";
    } else {
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

        if (expr.kind_ == NodeKind::FUNCTION_CALL) {
            const auto& funcCall = as<FunctionCall>(expr);
            std::cout << prefix << branch << "FunctionCall\n";
            std::cout << newPrefix << "└── Identifier: " << funcCall.identifier_->name_ << "\n";
        } else if (expr.kind_ == NodeKind::BINARY_EXPRESSION) {
            const auto& binaryExpr = as<BinaryExpression>(expr);
            std::cout << prefix << branch << "BinaryExpression\n";
            log_expression(*binaryExpr.left_, newPrefix, false);
            std::cout << newPrefix << "├── Operator: " << operator_to_string(binaryExpr.operator_)
                      << "\n";
            log_expression(*binaryExpr.right_, newPrefix, true);
        } else if (expr.kind_ == NodeKind::UNARY_EXPRESSION) {
            const auto& unaryExpr = as<UnaryExpression>(expr);
            std::cout << prefix << branch << "UnaryExpression\n";
            std::cout << newPrefix << "├── Operator: " << operator_to_string(unaryExpr.operator_)
                      << "\n";
            log_expression(*unaryExpr.operand_, newPrefix, true);
        }
    }
}

void log_statement(const Statement& stmt, const std::string& prefix, const bool isLast) {
    const std::string newPrefix = next_prefix(prefix, isLast);
    const std::string branch = isLast ? "└── " : "├── ";

    switch (stmt.kind_) {
        case NodeKind::ASSIGNMENT: {
            const auto& assignment = as<Assignment>(stmt);
            std::cout << prefix << branch
                      << (assignment.isDeclaration_ ? "Declaration Assignment\n" : "Assignment\n");
            std::cout << newPrefix << "├── Identifier: " << assignment.identifier_->name_ << "\n";
            std::cout << newPrefix << "└── Value\n";
            log_expression(*assignment.value_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = as<ExpressionStatement>(stmt);
            std::cout << prefix << branch << "ExpressionStatement\n";
            std::cout << newPrefix << "└── Expression\n";
            log_expression(*exprStmt.expression_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::IF_STATEMENT: {
            const auto& ifStmt = as<IfStatement>(stmt);
            std::cout << prefix << branch << "IfStatement\n";
            std::cout << newPrefix << "├── Condition\n";
            log_expression(*ifStmt.condition_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << (ifStmt.elseClause_ ? "├── " : "└── ") << "Body\n";
            log_statement(*ifStmt.body_, next_prefix(newPrefix, !ifStmt.elseClause_),
                          !ifStmt.elseClause_);
            if (ifStmt.elseClause_) {
                std::cout << newPrefix << "└── Else\n";
                log_statement(*ifStmt.elseClause_, next_prefix(newPrefix, true), true);
            }
            break;
        }
        case NodeKind::WHILE_STATEMENT: {
            const auto& whileStmt = as<WhileStatement>(stmt);
            std::cout << prefix << branch << "WhileStatement\n";
            std::cout << newPrefix << "├── Condition\n";
            log_expression(*whileStmt.condition_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "└── Body\n";
            log_statement(*whileStmt.body_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::FUNCTION_DECLARATION: {
            const auto& funcDecl = as<FunctionDeclaration>(stmt);
            std::cout << prefix << branch << "FunctionDeclaration\n";
            std::cout << newPrefix << "├── Identifier: " << funcDecl.identifier_->name_ << "\n";
            std::cout << newPrefix << "└── Body\n";
            log_statement(*funcDecl.body_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::EXIT: {
            const auto& exit = as<Exit>(stmt);
            std::cout << prefix << branch << "Exit\n";
            std::cout << newPrefix << "└── ExitCode\n";
            log_expression(*exit.exitCode_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = as<BlockStatement>(stmt);
            std::cout << prefix << branch << "BlockStatement\n";
            for (size_t i = 0; i < blockStmt.body_.size(); ++i) {
                const auto& innerStmt = blockStmt.body_[i];
                log_statement(*innerStmt, newPrefix, i == blockStmt.body_.size() - 1);
            }
            break;
        }
        default:
            throw std::invalid_argument("Invalid statement kind");
    }
}

}  // namespace

void log_ast(const Program& programNode) {
    std::cout << "Program\n";
    const auto& stmt = programNode.body_;
    log_statement(*stmt, "", true);
}

}  // namespace AST