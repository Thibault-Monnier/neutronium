#pragma once

#include <cassert>
#include <cstdint>
#include <span>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "ast/AST.hpp"
#include "type/TypeID.hpp"

enum class SymbolKind : uint8_t { FUNCTION, VARIABLE };

class SymbolInfo {
    const AST::Node* declarationNode_;

   public:
    explicit SymbolInfo(const AST::Node* declarationNode) : declarationNode_(declarationNode) {}

    /// Get the kind of symbol.
    [[nodiscard]] SymbolKind kind() const {
        switch (declarationNode_->kind_) {
            case AST::NodeKind::FUNCTION_DEFINITION:
            case AST::NodeKind::EXTERNAL_FUNCTION_DECLARATION:
                return SymbolKind::FUNCTION;
            case AST::NodeKind::VARIABLE_DEFINITION:
                return SymbolKind::VARIABLE;
            default:
                std::unreachable();
        }
    }

    /// Check if the variable is mutable. Only valid for variable symbols.
    [[nodiscard]] bool isMutable() const {
        assert(kind() == SymbolKind::VARIABLE);
        const auto* varDef = declarationNode_->as<const AST::VariableDefinition>();
        return varDef->isMutable();
    }

    /// Get the parameters of the function. Only valid for function symbols.
    [[nodiscard]] std::span<AST::VariableDefinition*> parameters() const {
        assert(kind() == SymbolKind::FUNCTION && "Only function symbols have parameters");
        switch (declarationNode_->kind_) {
            case AST::NodeKind::FUNCTION_DEFINITION: {
                const auto* funcDef = declarationNode_->as<const AST::FunctionDefinition>();
                return funcDef->parameters_;
            }
            case AST::NodeKind::EXTERNAL_FUNCTION_DECLARATION: {
                const auto* extFuncDecl =
                    declarationNode_->as<const AST::ExternalFunctionDeclaration>();
                return extFuncDecl->parameters_;
            }
            default:
                std::unreachable();
        }
    }

    /// Get the TypeID of the symbol.
    [[nodiscard]] TypeID typeID() const {
        switch (kind()) {
            case SymbolKind::FUNCTION: {
                switch (declarationNode_->kind_) {
                    case AST::NodeKind::FUNCTION_DEFINITION: {
                        const auto* funcDef = declarationNode_->as<const AST::FunctionDefinition>();
                        return funcDef->returnTypeID_;
                    }
                    case AST::NodeKind::EXTERNAL_FUNCTION_DECLARATION: {
                        const auto* extFuncDecl =
                            declarationNode_->as<const AST::ExternalFunctionDeclaration>();
                        return extFuncDecl->returnTypeID_;
                    }
                    default:
                        std::unreachable();
                }
            }
            case SymbolKind::VARIABLE: {
                const auto* varDef = declarationNode_->as<const AST::VariableDefinition>();
                return varDef->typeID_;
            }
            default:
                std::unreachable();
        }
    }
};

using SymbolTable = std::unordered_map<std::string_view, SymbolInfo>;
