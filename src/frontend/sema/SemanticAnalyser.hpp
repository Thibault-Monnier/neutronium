#pragma once

#include <string_view>

#include "SymbolTable.hpp"
#include "driver/Cli.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/diagnostics/DiagnosticsEngine.hpp"
#include "frontend/source/FileID.hpp"
#include "frontend/type/Trait.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeManager.hpp"
#include "typeInfer/ConstraintGenerator.hpp"
#include "validator/Validator.hpp"

namespace Sema {

class SemanticAnalyser {
   public:
    explicit SemanticAnalyser(const AST::CompilationUnit& ast, const TargetType targetType,
                              DiagnosticsEngine& diagnosticsEngine, const FileID fileID,
                              TypeManager& typeManager, TypeSolver& typeSolver)
        : ast_(&ast),
          targetType_(targetType),
          diagnosticsEngine_(diagnosticsEngine),
          fileID_(fileID),
          typeManager_(typeManager),
          typeSolver_(typeSolver) {}

    void analyse();

   private:
    const AST::CompilationUnit* ast_;
    const TargetType targetType_;

    DiagnosticsEngine& diagnosticsEngine_;
    const FileID fileID_;

    TypeManager& typeManager_;

    SymbolTableMap functionsTable_;

    TypeSolver& typeSolver_;  // Instantiated in the driver to better manage memory.
    ConstraintGenerator constraintGenerator_{*ast_,        diagnosticsEngine_, fileID_,
                                             typeManager_, typeSolver_,        functionsTable_};
    Validator validator_{*ast_,   targetType_,  diagnosticsEngine_,
                         fileID_, typeManager_, functionsTable_};

    static constexpr std::string_view ENTRY_POINT_NAME = "main";

    void emitErrorsAndQuitIfNeeded() const {
        if (diagnosticsEngine_.hasErrors()) emitErrorsAndQuit();
    }
    void emitErrorsAndQuit() const;
};

}  // namespace Sema
