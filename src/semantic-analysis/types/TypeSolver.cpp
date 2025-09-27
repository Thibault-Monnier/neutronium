#include "semantic-analysis/types/TypeSolver.hpp"

#include "semantic-analysis/types/TypeManager.hpp"

void TypeSolver::solveEqualityConstraints() const {
    // Uses the union-find algorithm to solve equality constraints

    struct Node {
        TypeID parent_;
        int setSize_;
    };

    std::vector<Node> nodes(typeManager_.getTypeCount());
    for (TypeID i = 0; i < nodes.size(); ++i) {
        nodes[i] = {.parent_ = i, .setSize_ = 1};
    }

    const auto find = [&](TypeID x) {
        TypeID root = x;
        // Find the root
        while (root != nodes[root].parent_) root = nodes[root].parent_;

        // Path compression
        while (x != root) {
            const TypeID parent = nodes[x].parent_;
            nodes[x].parent_ = root;
            x = parent;
        }

        return root;
    };

    for (const auto& constraint : constraints_) {
        if (constraint->kind() != Constraint::Kind::EQUALITY) {
            continue;
        }

        const auto& equalityConstraint = static_cast<const EqualityConstraint&>(*constraint);
        const TypeID a = equalityConstraint.a();
        const TypeID b = equalityConstraint.b();

        const TypeID rootA = find(a);
        const TypeID rootB = find(b);

        if (rootA != rootB) {
            const auto unionSets = [&](const TypeID x, const TypeID y) {
                nodes[y].parent_ = x;
                nodes[x].setSize_ += nodes[y].setSize_;
            };

            if (nodes[rootA].setSize_ >= nodes[rootB].setSize_)
                unionSets(rootA, rootB);
            else
                unionSets(rootB, rootA);
        }
    }

    for (TypeID node = 0; node < nodes.size(); ++node) {
        const TypeID root = find(node);
        if (root == node) continue;

        // const Type& nodeType = typeManager_.getType(node);
        // Type& rootType = typeManager_.getType(root);
        // rootType.mergeWith(nodeType);

        typeManager_.linkTypes(root, node);
    }
}

void TypeSolver::solveHasTraitConstraints() {
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
