#include <cstdlib>
#include <format>

#include "TypeSolver.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/type/Trait.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeID.hpp"
#include "frontend/type/TypeManager.hpp"

void TypeSolver::prepareEmitError() {
    // Before emitting an error, ensure the type manager is up to date to get accurate type
    // information in error messages.
    linkAllNodes();
}

__attribute__((cold, noinline)) void TypeSolver::equalityConstraintError(
    const TypeID a, const TypeID b, const AST::Node& sourceNode) {
    prepareEmitError();

    // Safe because there can't be a solving error for type variables
    const Type& aType = typeManager_.getTypeResolved(a);
    const Type& bType = typeManager_.getTypeResolved(b);

    diagnosticsEngine_.reportError(
        std::format("Type mismatch: cannot unify types '{}' and '{}'", aType.toString(typeManager_),
                    bType.toString(typeManager_)),
        sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(), sourceNode.fileID());
    diagnosticsEngine_.emit();

    exit(EXIT_FAILURE);
}

__attribute__((cold, noinline)) void TypeSolver::hasTraitConstraintError(
    const Type& type, const Trait trait, const AST::Node& sourceNode) {
    prepareEmitError();

    diagnosticsEngine_.reportError(std::format("Type '{}' does not implement the trait '{}'",
                                               type.toString(typeManager_), traitToString(trait)),
                                   sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(),
                                   sourceNode.fileID());
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}

__attribute__((cold, noinline)) void TypeSolver::storableConstraintError(
    const Type& type, const AST::Node& sourceNode) {
    prepareEmitError();

    diagnosticsEngine_.reportError(
        std::format("Type '{}' is not storable", type.toString(typeManager_)),
        sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(), sourceNode.fileID());
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}
