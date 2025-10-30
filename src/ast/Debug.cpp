#include "Debug.hpp"

#include <iostream>
#include <magic_enum/magic_enum.hpp>
#include <unordered_map>
#include <utility>

namespace AST {

std::string operator_to_string(const Operator op) {
    static const std::unordered_map<Operator, std::string> table = {
        {Operator::ADD, "+"},
        {Operator::SUBTRACT, "-"},
        {Operator::MULTIPLY, "*"},
        {Operator::DIVIDE, "/"},
        {Operator::LOGICAL_NOT, "!"},
        {Operator::ASSIGN, "="},
        {Operator::ADD_ASSIGN, "+="},
        {Operator::SUBTRACT_ASSIGN, "-="},
        {Operator::MULTIPLY_ASSIGN, "*="},
        {Operator::DIVIDE_ASSIGN, "/="},
        {Operator::EQUALS, "=="},
        {Operator::NOT_EQUALS, "!="},
        {Operator::LESS_THAN, "<"},
        {Operator::LESS_THAN_OR_EQUAL, "<="},
        {Operator::GREATER_THAN, ">"},
        {Operator::GREATER_THAN_OR_EQUAL, ">="},
    };

    const auto it = table.find(op);
    assert(it != table.end() && "Invalid operator");
    return it->second;
}

std::string node_kind_to_string(const NodeKind kind) {
    const auto enumName = magic_enum::enum_name(kind);
    return std::string{enumName};
}

// =========================================== //
// ============= Log AST helpers ============= //
// =========================================== //

std::string next_prefix(const std::string& p, const bool isLast) {
    return p + (isLast ? "    " : "│   ");
}

void log_expression(const Expression& expr, const std::string& prefix, const bool isLast) {
    std::string branch = "├── ";

    if (expr.kind_ == NodeKind::NUMBER_LITERAL) {
        const auto& numberLit = *expr.as<NumberLiteral>();
        std::cout << prefix << branch << "NumberLiteral: " << numberLit.value_ << "\n";
    } else if (expr.kind_ == NodeKind::BOOLEAN_LITERAL) {
        const auto& booleanLit = *expr.as<BooleanLiteral>();
        std::cout << prefix << branch
                  << "BooleanLiteral: " << (booleanLit.value_ ? "true" : "false") << "\n";
    } else if (expr.kind_ == NodeKind::IDENTIFIER) {
        const auto& identifier = *expr.as<Identifier>();
        std::cout << prefix << branch << "Identifier: " << identifier.name_ << "\n";
    } else {
        const std::string newPrefix = prefix + "│   ";

        if (expr.kind_ == NodeKind::ARRAY_LITERAL) {
            const auto& arrayLit = *expr.as<ArrayLiteral>();
            std::cout << prefix << branch << "ArrayLiteral\n";
            for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
                const auto& element = arrayLit.elements_[i];
                const bool isLastElement = i == arrayLit.elements_.size() - 1;
                const std::string elementBranch = isLastElement ? "└── " : "├── ";
                std::cout << newPrefix << elementBranch << "Element" << i + 1 << "\n";
                log_expression(*element, next_prefix(newPrefix, isLastElement), true);
            }

        } else if (expr.kind_ == NodeKind::ARRAY_ACCESS) {
            const auto& arrayAccess = *expr.as<ArrayAccess>();
            std::cout << prefix << branch << "ArrayAccess\n";
            std::cout << newPrefix << "├── Base\n";
            log_expression(*arrayAccess.base_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "└── Index\n";
            log_expression(*arrayAccess.index_, next_prefix(newPrefix, true), true);

        } else if (expr.kind_ == NodeKind::FUNCTION_CALL) {
            const auto& funcCall = *expr.as<FunctionCall>();
            std::cout << prefix << branch << "FunctionCall\n";
            std::cout << newPrefix << "├── Callee\n";
            log_expression(*funcCall.callee_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "└── Arguments\n";
            for (size_t i = 0; i < funcCall.arguments_.size(); ++i) {
                const auto& arg = funcCall.arguments_[i];
                const bool isLastArg = i == funcCall.arguments_.size() - 1;
                const std::string argPrefix = next_prefix(newPrefix, true);
                const std::string argBranch = isLastArg ? "└── " : "├── ";
                std::cout << argPrefix << argBranch << "Argument" << i + 1 << "\n";
                log_expression(*arg, next_prefix(argPrefix, isLastArg), true);
            }
        } else if (expr.kind_ == NodeKind::BINARY_EXPRESSION) {
            const auto& binaryExpr = *expr.as<BinaryExpression>();
            std::cout << prefix << branch << "BinaryExpression\n";
            log_expression(*binaryExpr.left_, newPrefix, false);
            std::cout << newPrefix << "├── Operator: " << operator_to_string(binaryExpr.operator_)
                      << "\n";
            log_expression(*binaryExpr.right_, newPrefix, true);
        } else if (expr.kind_ == NodeKind::UNARY_EXPRESSION) {
            const auto& unaryExpr = *expr.as<UnaryExpression>();
            std::cout << prefix << branch << "UnaryExpression\n";
            std::cout << newPrefix << "├── Operator: " << operator_to_string(unaryExpr.operator_)
                      << "\n";
            log_expression(*unaryExpr.operand_, newPrefix, true);
        }
    }

    branch = isLast ? "└── " : "├── ";
    std::cout << prefix << branch << "TypeID: " << expr.typeID_ << "\n";
}

void log_statement(const Statement& stmt, const std::string& prefix, const bool isLast) {
    const std::string newPrefix = next_prefix(prefix, isLast);
    const std::string branch = isLast ? "└── " : "├── ";

    switch (stmt.kind_) {
        case NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = *stmt.as<VariableDefinition>();
            std::cout << prefix << branch << "VariableDefinition\n";
            std::cout << newPrefix << "├── Identifier: " << varDecl.identifier_->name_ << "\n";
            std::cout << newPrefix << "├── TypeID: " << varDecl.typeID_ << "\n";
            std::cout << newPrefix << "├── IsMutable: " << (varDecl.isMutable_ ? "true" : "false")
                      << "\n";
            std::cout << newPrefix << "└── Value\n";
            log_expression(*varDecl.value_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::ASSIGNMENT: {
            const auto& assignment = *stmt.as<Assignment>();
            std::cout << prefix << branch << "Assignment\n";
            std::cout << newPrefix << "├── Place\n";
            log_expression(*assignment.place_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "├── Operator: " << operator_to_string(assignment.operator_)
                      << "\n";
            std::cout << newPrefix << "└── Value\n";
            log_expression(*assignment.value_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::EXPRESSION_STATEMENT: {
            const auto& exprStmt = *stmt.as<ExpressionStatement>();
            std::cout << prefix << branch << "ExpressionStatement\n";
            std::cout << newPrefix << "└── Expression\n";
            log_expression(*exprStmt.expression_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::IF_STATEMENT: {
            const auto& ifStmt = *stmt.as<IfStatement>();
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
            const auto& whileStmt = *stmt.as<WhileStatement>();
            std::cout << prefix << branch << "WhileStatement\n";
            std::cout << newPrefix << "├── Condition\n";
            log_expression(*whileStmt.condition_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "└── Body\n";
            log_statement(*whileStmt.body_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::BREAK_STATEMENT:
            std::cout << prefix << branch << "BreakStatement\n";
            break;
        case NodeKind::CONTINUE_STATEMENT:
            std::cout << prefix << branch << "ContinueStatement\n";
            break;
        case NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = *stmt.as<ReturnStatement>();
            std::cout << prefix << branch << "ReturnStatement\n";
            if (returnStmt.returnValue_) {
                std::cout << newPrefix << "└── Value\n";
                log_expression(*returnStmt.returnValue_, next_prefix(newPrefix, true), true);
            }
            break;
        }
        case NodeKind::EXIT_STATEMENT: {
            const auto& exit = *stmt.as<ExitStatement>();
            std::cout << prefix << branch << "Exit\n";
            std::cout << newPrefix << "└── ExitCode\n";
            log_expression(*exit.exitCode_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = *stmt.as<BlockStatement>();
            std::cout << prefix << branch << "BlockStatement\n";
            for (size_t i = 0; i < blockStmt.body_.size(); ++i) {
                const auto& innerStmt = blockStmt.body_[i];
                log_statement(*innerStmt, newPrefix, i == blockStmt.body_.size() - 1);
            }
            break;
        }
        default:
            std::unreachable();
    }

    if (!isLast) {
        std::cout << prefix << "│\n";
    }
}

void log_ast(const Program& programNode) {
    std::cout << "Program\n";

    const std::string prefix = "    ";

    const auto functionSignature =
        [&](const Identifier& identifier,
            const std::vector<std::unique_ptr<VariableDefinition>>& params,
            const TypeID returnTypeID, const std::string& newPrefix, const bool hasBody) {
            std::cout << newPrefix << "├── Identifier: " << identifier.name_ << "\n";
            std::cout << newPrefix << "├── ReturnTypeID: " << returnTypeID << "\n";

            const std::string parametersBranch = hasBody ? "├── " : "└── ";
            std::cout << newPrefix << parametersBranch << "Parameters\n";
            const std::string paramsPrefix = next_prefix(newPrefix, !hasBody);
            for (size_t j = 0; j < params.size(); ++j) {
                const auto& param = params[j];

                const std::string paramBranch = j == params.size() - 1 ? "└── " : "├── ";
                std::cout << paramsPrefix << paramBranch << "Parameter" << j + 1 << "\n";
                std::cout << next_prefix(paramsPrefix, j == params.size() - 1)
                          << "├── Identifier: " << param->identifier_->name_ << "\n";
                std::cout << next_prefix(paramsPrefix, j == params.size() - 1)
                          << "├── TypeID: " << param->typeID_ << "\n";
                std::cout << next_prefix(paramsPrefix, j == params.size() - 1)
                          << "└── IsMutable: " << (param->isMutable_ ? "true" : "false") << "\n";
            }
        };

    for (size_t i = 0; i < programNode.externalFunctions_.size(); ++i) {
        const bool isLast =
            i == programNode.externalFunctions_.size() - 1 && programNode.functions_.empty();
        const std::string branch = isLast ? "└── " : "├── ";
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");
        const auto& externFunc =
            *programNode.externalFunctions_[i]->as<ExternalFunctionDeclaration>();

        std::cout << prefix << branch << "ExternalFunctionDeclaration\n";
        functionSignature(*externFunc.identifier_, externFunc.parameters_, externFunc.returnTypeID_,
                          newPrefix, false);

        if (!isLast) {
            std::cout << prefix << "│\n";
        }
    }

    for (size_t i = 0; i < programNode.functions_.size(); ++i) {
        const bool isLast = i == programNode.functions_.size() - 1;
        const std::string branch = isLast ? "└── " : "├── ";
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");
        const auto& funcDef = *programNode.functions_[i]->as<FunctionDefinition>();

        std::cout << prefix << branch << "FunctionDefinition\n";
        functionSignature(*funcDef.identifier_, funcDef.parameters_, funcDef.returnTypeID_,
                          newPrefix, true);
        std::cout << newPrefix << "├── IsExported: " << (funcDef.isExported_ ? "true" : "false")
                  << "\n";

        std::cout << newPrefix << "└── Body\n";
        log_statement(*funcDef.body_, next_prefix(newPrefix, true), true);
        if (!isLast) {
            std::cout << prefix << "│\n";
        }
    }
}

}  // namespace AST
