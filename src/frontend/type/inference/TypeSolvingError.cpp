#include "TypeSolver.hpp"
#include "ast/AST.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

__attribute__((cold, noinline)) void TypeSolver::equalityConstraintError(
    const TypeID a, const TypeID b, const AST::Node& sourceNode) const {
    const Type& aType = typeManager_.getType(a);
    const Type& bType = typeManager_.getType(b);

    diagnosticsEngine_.reportError(
        std::format("Type mismatch: cannot unify types '{}' and '{}'", aType.toString(typeManager_),
                    bType.toString(typeManager_)),
        sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(), sourceNode.fileID());
    diagnosticsEngine_.emit();

    exit(EXIT_FAILURE);
}

__attribute__((cold, noinline)) void TypeSolver::hasTraitConstraintError(
    const Type& type, const Trait trait, const AST::Node& sourceNode) const {
    diagnosticsEngine_.reportError(std::format("Type '{}' does not implement the trait '{}'",
                                               type.toString(typeManager_), traitToString(trait)),
                                   sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(),
                                   sourceNode.fileID());
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}

__attribute__((cold, noinline)) void TypeSolver::storableConstraintError(
    const Type& type, const AST::Node& sourceNode) const {
    diagnosticsEngine_.reportError(
        std::format("Type '{}' is not storable", type.toString(typeManager_)),
        sourceNode.sourceStartIndex(), sourceNode.sourceEndIndex(), sourceNode.fileID());
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}
