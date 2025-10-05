#include "semantic-analysis/types/TypeSolver.hpp"

#include "semantic-analysis/types/TypeManager.hpp"

TypeID TypeSolver::findRoot(TypeID x) {
    // Find the root of the set containing x
    TypeID root = x;
    while (root != nodes_[root].parent_) root = nodes_[root].parent_;

    // Path compression
    while (x != root) {
        const TypeID parent = nodes_[x].parent_;
        nodes_[x].parent_ = root;
        x = parent;
    }

    return root;
}

bool TypeSolver::unify(const TypeID dst, const TypeID src) {
    nodes_[src].parent_ = dst;
    nodes_[dst].setSize_ += nodes_[src].setSize_;

    Type& dstType = typeManager_.getType(dst);
    const Type& srcType = typeManager_.getType(src);

    // The following modifies dstType only, but it is fine since src will never be root
    // again, so we don't care about its type anymore

    if (dstType.isUnknownKind() || srcType.isUnknownKind()) return dstType.mergeWith(srcType);

    if (dstType.kind() != srcType.kind()) return false;

    switch (dstType.kind()) {
        case TypeKind::UNKNOWN:
            std::unreachable();
        case TypeKind::PRIMITIVE:
            return dstType.mergeWith(srcType);
        case TypeKind::ARRAY:
            if (!dstType.matches(srcType)) return false;
            return unify(dstType.array_element_type_id(), srcType.array_element_type_id());
    }
    std::unreachable();
}

void TypeSolver::solveEqualityConstraints() {
    // Uses a union-find algorithm to solve equality constraints

    nodes_.resize(typeManager_.getTypeCount());

    for (TypeID i = 0; i < nodes_.size(); ++i) {
        nodes_[i] = {.parent_ = i, .setSize_ = 1};
    }

    for (const auto& constraint : constraints_) {
        if (constraint->kind() != Constraint::Kind::EQUALITY) {
            continue;
        }

        const auto& equalityConstraint = static_cast<const EqualityConstraint&>(*constraint);
        const TypeID a = equalityConstraint.a();
        const TypeID b = equalityConstraint.b();

        TypeID rootA = findRoot(a);
        TypeID rootB = findRoot(b);

        if (rootA == rootB) continue;

        if (nodes_[rootA].setSize_ < nodes_[rootB].setSize_) std::swap(rootA, rootB);

        if (!unify(rootA, rootB)) {
            // Types are not compatible
            const Type& aType = typeManager_.getType(rootA);
            const Type& bType = typeManager_.getType(rootB);

            diagnosticsEngine_.report_error(
                std::format("Type mismatch: cannot unify types '{}' and '{}'", aType.to_string(),
                            bType.to_string()),
                equalityConstraint.sourceNode().source_start_index(),
                equalityConstraint.sourceNode().source_end_index());
            diagnosticsEngine_.emit_errors();
            exit(EXIT_FAILURE);
        }
    }

    for (TypeID node = 0; node < nodes_.size(); ++node) {
        const TypeID root = findRoot(node);
        if (root == node) continue;

        typeManager_.linkTypes(root, node);
    }
}

void TypeSolver::solveHasTraitConstraints() const {
    for (const auto& constraint : constraints_) {
        if (constraint->kind() != Constraint::Kind::HAS_TRAIT) {
            continue;
        }

        const auto& hasTraitConstraint = static_cast<const HasTraitConstraint&>(*constraint);
        Type& type = typeManager_.getType(hasTraitConstraint.type());
        const Trait& trait = hasTraitConstraint.trait();
        type.addTrait(trait);
    }
}

void TypeSolver::solve() {
    solveEqualityConstraints();
    solveHasTraitConstraints();
}
