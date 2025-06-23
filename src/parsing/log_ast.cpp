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

        if (expr.kind_ == NodeKind::ARRAY_LITERAL) {
            const auto& arrayLit = as<ArrayLiteral>(expr);
            std::cout << prefix << branch << "ArrayLiteral\n";
            for (size_t i = 0; i < arrayLit.elements_.size(); ++i) {
                const auto& element = arrayLit.elements_[i];
                const bool isLastElement = i == arrayLit.elements_.size() - 1;
                const std::string elementBranch = isLastElement ? "└── " : "├── ";
                std::cout << newPrefix << elementBranch << "Element" << i + 1 << "\n";
                log_expression(*element, next_prefix(newPrefix, isLastElement), true);
            }

        } else if (expr.kind_ == NodeKind::ARRAY_ACCESS) {
            const auto& arrayAccess = as<ArrayAccess>(expr);
            std::cout << prefix << branch << "ArrayAccess\n";
            std::cout << newPrefix << "├── Identifier: " << arrayAccess.identifier_->name_ << "\n";
            std::cout << newPrefix << "└── Index\n";
            log_expression(*arrayAccess.index_, next_prefix(newPrefix, true), true);

        } else if (expr.kind_ == NodeKind::FUNCTION_CALL) {
            const auto& funcCall = as<FunctionCall>(expr);
            std::cout << prefix << branch << "FunctionCall\n";
            std::cout << newPrefix << "├── Identifier: " << funcCall.identifier_->name_ << "\n";
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
        case NodeKind::VARIABLE_DEFINITION: {
            const auto& varDecl = as<VariableDefinition>(stmt);
            std::cout << prefix << branch << "VariableDefinition\n";
            std::cout << newPrefix << "├── Identifier: " << varDecl.identifier_->name_ << "\n";
            std::cout << newPrefix << "├── Type: " << varDecl.type_.to_string() << "\n";
            std::cout << newPrefix << "├── IsMutable: " << (varDecl.isMutable_ ? "true" : "false")
                      << "\n";
            std::cout << newPrefix << "└── Value\n";
            log_expression(*varDecl.value_, next_prefix(newPrefix, true), true);
            break;
        }
        case NodeKind::ASSIGNMENT: {
            const auto& assignment = as<Assignment>(stmt);
            std::cout << prefix << branch << "Assignment\n";
            std::cout << newPrefix << "├── Left\n";
            log_expression(*assignment.left_, next_prefix(newPrefix, false), true);
            std::cout << newPrefix << "└── Right\n";
            log_expression(*assignment.right_, next_prefix(newPrefix, true), true);
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
        case NodeKind::BREAK_STATEMENT:
            std::cout << prefix << branch << "BreakStatement\n";
            break;
        case NodeKind::CONTINUE_STATEMENT:
            std::cout << prefix << branch << "ContinueStatement\n";
            break;
        case NodeKind::RETURN_STATEMENT: {
            const auto& returnStmt = as<ReturnStatement>(stmt);
            std::cout << prefix << branch << "ReturnStatement\n";
            if (returnStmt.returnValue_) {
                std::cout << newPrefix << "└── Value\n";
                log_expression(*returnStmt.returnValue_, next_prefix(newPrefix, true), true);
            }
            break;
        }
        case NodeKind::EXIT_STATEMENT: {
            const auto& exit = as<ExitStatement>(stmt);
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

    if (!isLast) {
        std::cout << prefix << "│\n";
    }
}

}  // namespace

void log_ast(const Program& programNode) {
    std::cout << "Program\n";

    const std::string prefix = "    ";

    const auto functionSignature =
        [](const AST::Identifier& identifier,
           const std::vector<std::unique_ptr<AST::VariableDefinition>>& params,
           const Type& returnType, const std::string& newPrefix, bool hasBody) {
            std::cout << newPrefix << "├── Identifier: " << identifier.name_ << "\n";
            std::cout << newPrefix << "├── ReturnType: " << returnType.to_string() << "\n";

            const std::string parametersBranch = hasBody ? "├── " : "└── ";
            std::cout << newPrefix << parametersBranch << "Parameters\n";
            for (size_t j = 0; j < params.size(); ++j) {
                const auto& param = params[j];
                const std::string paramPrefix =
                    next_prefix(newPrefix, (j == params.size() - 1) && !hasBody);
                const std::string paramBranch = j == params.size() - 1 ? "└── " : "├── ";
                std::cout << paramPrefix << paramBranch << "Parameter" << j + 1 << "\n";
                std::cout << next_prefix(paramPrefix, j == params.size() - 1)
                          << "├── Identifier: " << param->identifier_->name_ << "\n";
                std::cout << next_prefix(paramPrefix, j == params.size() - 1)
                          << "├── Type: " << param->type_.to_string() << "\n";
                std::cout << next_prefix(paramPrefix, j == params.size() - 1)
                          << "└── IsMutable: " << (param->isMutable_ ? "true" : "false") << "\n";
            }
        };

    for (size_t i = 0; i < programNode.externalFunctions_.size(); ++i) {
        const bool isLast = i == programNode.externalFunctions_.size() - 1 &&
                            programNode.constants_.empty() && programNode.functions_.empty();
        const std::string branch = isLast ? "└── " : "├── ";
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");
        const auto& externFunc =
            as<ExternalFunctionDeclaration>(*programNode.externalFunctions_[i]);

        std::cout << prefix << branch << "ExternalFunctionDeclaration\n";
        functionSignature(*externFunc.identifier_, externFunc.parameters_, externFunc.returnType_,
                          newPrefix, false);

        if (!isLast) {
            std::cout << prefix << "│\n";
        }
    }

    for (size_t i = 0; i < programNode.constants_.size(); ++i) {
        const bool isLast =
            i == programNode.constants_.size() - 1 && programNode.functions_.empty();
        const std::string branch = isLast ? "└── " : "├── ";
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");

        const auto& constant = as<ConstantDefinition>(*programNode.constants_[i]);

        std::cout << prefix << branch << "ConstantDefinition\n";
        std::cout << newPrefix << "├── Identifier: " << constant.identifier_->name_ << "\n";
        std::cout << newPrefix << "├── Type: " << constant.type_.to_string() << "\n";
        std::cout << newPrefix << "└── Value\n";
        log_expression(*constant.value_, next_prefix(newPrefix, true), true);
        if (!isLast) {
            std::cout << prefix << "│\n";
        }
    }

    for (size_t i = 0; i < programNode.functions_.size(); ++i) {
        const bool isLast = i == programNode.functions_.size() - 1;
        const std::string branch = isLast ? "└── " : "├── ";
        const std::string newPrefix = prefix + (isLast ? "    " : "│   ");
        const auto& funcDef = as<FunctionDefinition>(*programNode.functions_[i]);

        std::cout << prefix << branch << "FunctionDefinition\n";
        functionSignature(*funcDef.identifier_, funcDef.parameters_, funcDef.returnType_, newPrefix,
                          true);
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