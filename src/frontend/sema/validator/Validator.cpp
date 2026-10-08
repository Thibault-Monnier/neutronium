#include "Validator.hpp"

#include "frontend/sema/SymbolTable.hpp"

using namespace Sema;

void Validator::traverse() {
    // Nothing to check for external functions.

    for (const auto& funcDef : ast_->functions_) {
        analyseFunctionDefinition(*funcDef);
    }

    const auto mainIt = functionsTable_.find(ENTRY_POINT_NAME);
    if (targetType_ == TargetType::EXECUTABLE) {
        if (mainIt == functionsTable_.end() || mainIt->second.kind() != SymbolKind::FUNCTION) {
            error(std::format("No `{}` function found in executable target", ENTRY_POINT_NAME),
                  *ast_);
        } else {
            const SymbolInfo info = mainIt->second;

            if (info.parameters().size() != 0) {
                error(std::format("`{}` function must not take any parameters", ENTRY_POINT_NAME),
                      *info.declarationNode());
            }

            if (!typeManager_.getTypeResolved(info.typeID()).isVoid()) {
                error(std::format("`{}` function must return void", ENTRY_POINT_NAME),
                      *info.declarationNode());
            }
        }
    } else {
        if (mainIt != functionsTable_.end()) {
            error(std::format("`{}` function is not allowed in a library target", ENTRY_POINT_NAME),
                  *mainIt->second.declarationNode());
        }
    }
}

void Validator::error(const std::string& errorMessage, const AST::Node& node) const {
    diagnosticsEngine_.reportError(errorMessage, node.sourceStartIndex(), node.sourceEndIndex(),
                                   fileID_);
}

void Validator::checkExpression(const AST::Expression& expr) {
    switch (expr.kind_) {
        case AST::NodeKind::NUMBER_LITERAL:
        case AST::NodeKind::BOOLEAN_LITERAL:
        case AST::NodeKind::CHARACTER_LITERAL:
            break;

        case AST::NodeKind::ARRAY_LITERAL: {
            const auto& arrayLiteral = *expr.as<AST::ArrayLiteral>();
            for (const auto* element : arrayLiteral.elements_) checkExpression(*element);
            break;
        }

        case AST::NodeKind::REPEAT_ARRAY_LITERAL: {
            const auto& repeatArrayLit = *expr.as<AST::RepeatArrayLiteral>();
            checkExpression(*repeatArrayLit.count_);
            checkExpression(*repeatArrayLit.element_);
            break;
        }

        case AST::NodeKind::IDENTIFIER:
            break;

        case AST::NodeKind::ARRAY_ACCESS: {
            const auto& arrayAccess = *expr.as<AST::ArrayAccess>();
            checkExpression(*arrayAccess.base_);
            checkExpression(*arrayAccess.index_);
            break;
        }

        case AST::NodeKind::FUNCTION_CALL: {
            const auto& funcCall = *expr.as<AST::FunctionCall>();
            checkExpression(*funcCall.callee_);
            for (const auto* arg : funcCall.arguments_) checkExpression(*arg);
            break;
        }

        case AST::NodeKind::UNARY_EXPRESSION: {
            const auto& unaryExpr = *expr.as<AST::UnaryExpression>();
            checkExpression(*unaryExpr.operand_);
            break;
        }

        case AST::NodeKind::BINARY_EXPRESSION: {
            const auto& binaryExpr = *expr.as<AST::BinaryExpression>();
            checkExpression(*binaryExpr.left_);
            checkExpression(*binaryExpr.right_);
            break;
        }

        default:
            std::unreachable();
    }
}

void Validator::analyseVariableDefinition(const AST::VariableDefinition& definition) {
    checkExpression(*definition.identifier_);
    checkExpression(*definition.value_);

    // storableConstraint(definition.typeID_, definition);
    // TODO: ensure the variable is storable. Currently handled as a constraint.
}

void Validator::analyseAssignment(const AST::Assignment& assignment) {
    const auto* place = assignment.place_;

    // TODO: ensure the place is assignable. Currently handled in the ConstraintGenerator.

    checkExpression(*place);
    checkExpression(*assignment.value_);
}

void Validator::analyseExpressionStatement(const AST::ExpressionStatement& exprStmt) {
    checkExpression(*exprStmt.expression_);
}

void Validator::analyseIfStatement(const AST::IfStatement& ifStmt) {
    checkExpression(*ifStmt.condition_);
    analyseStatement(*ifStmt.body_);
    if (ifStmt.elseClause_) {
        analyseStatement(*ifStmt.elseClause_);
    }
}

void Validator::analyseWhileStatement(const AST::WhileStatement& whileStmt) {
    checkExpression(*whileStmt.condition_);
    loopDepth_++;
    analyseStatement(*whileStmt.body_);
    loopDepth_--;
}

void Validator::analyseBreakStatement(const AST::BreakStatement& breakStmt) const {
    if (loopDepth_ == 0) {
        error("`break` statement is not allowed outside of a loop", breakStmt);
    }
}

void Validator::analyseContinueStatement(const AST::ContinueStatement& continueStmt) const {
    if (loopDepth_ == 0) {
        error("`continue` statement is not allowed outside of a loop", continueStmt);
    }
}

void Validator::analyseReturnStatement(const AST::ReturnStatement& returnStmt) {
    if (returnStmt.returnValue_) {
        checkExpression(*returnStmt.returnValue_);
    }
}

void Validator::analyseExit(const AST::ExitStatement& exitStmt) {
    checkExpression(*exitStmt.exitCode_);
}

void Validator::analyseStatement(const AST::Statement& stmt) {
    switch (stmt.kind_) {
        case AST::NodeKind::VARIABLE_DEFINITION:
            analyseVariableDefinition(*stmt.as<AST::VariableDefinition>());
            break;
        case AST::NodeKind::ASSIGNMENT:
            analyseAssignment(*stmt.as<AST::Assignment>());
            break;
        case AST::NodeKind::EXPRESSION_STATEMENT:
            analyseExpressionStatement(*stmt.as<AST::ExpressionStatement>());
            break;
        case AST::NodeKind::IF_STATEMENT:
            analyseIfStatement(*stmt.as<AST::IfStatement>());
            break;
        case AST::NodeKind::WHILE_STATEMENT:
            analyseWhileStatement(*stmt.as<AST::WhileStatement>());
            break;
        case AST::NodeKind::BREAK_STATEMENT:
            analyseBreakStatement(*stmt.as<AST::BreakStatement>());
            break;
        case AST::NodeKind::CONTINUE_STATEMENT:
            analyseContinueStatement(*stmt.as<AST::ContinueStatement>());
            break;
        case AST::NodeKind::RETURN_STATEMENT:
            analyseReturnStatement(*stmt.as<AST::ReturnStatement>());
            break;
        case AST::NodeKind::EXIT_STATEMENT:
            analyseExit(*stmt.as<AST::ExitStatement>());
            break;
        case AST::NodeKind::BLOCK_STATEMENT: {
            const auto& blockStmt = *stmt.as<AST::BlockStatement>();
            for (const auto* innerStmt : blockStmt.body_) {
                analyseStatement(*innerStmt);
            }
            break;
        }
        default:
            std::unreachable();
    }
}

bool Validator::verifyStatementReturns(const AST::Statement& stmt) {
    const AST::NodeKind kind = stmt.kind_;

    if (kind == AST::NodeKind::IF_STATEMENT) {
        const auto& ifStmt = *stmt.as<AST::IfStatement>();
        if (!ifStmt.elseClause_) return false;

        const bool body = verifyStatementReturns(*ifStmt.body_);
        const bool elseBody = verifyStatementReturns(*ifStmt.elseClause_);
        return body && elseBody;

    } else if (kind == AST::NodeKind::BLOCK_STATEMENT) {
        const auto& blockStmt = *stmt.as<AST::BlockStatement>();
        for (const auto* innerStmt : blockStmt.body_) {
            if (verifyStatementReturns(*innerStmt)) return true;
        }

    } else if (kind == AST::NodeKind::RETURN_STATEMENT || kind == AST::NodeKind::EXIT_STATEMENT ||
               kind == AST::NodeKind::BREAK_STATEMENT) {
        return true;
    }

    return false;
}

void Validator::analyseFunctionDefinition(const AST::FunctionDefinition& funcDef) {
    analyseStatement(*funcDef.body_);

    const Type& returnType = typeManager_.getTypeResolved(funcDef.returnTypeID_);
    const bool allPathsReturn = verifyStatementReturns(*funcDef.body_);
    if (!allPathsReturn && !returnType.isVoid()) {
        error(
            std::format("Function `{}` must return a value of type {}, but does not always return",
                        funcDef.identifier_->name_, returnType.toString(typeManager_)),
            funcDef);
    }
}
