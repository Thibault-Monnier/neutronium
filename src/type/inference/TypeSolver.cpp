#include "TypeSolver.hpp"

#include "../TypeManager.hpp"
#include "ast/AST.hpp"

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

bool TypeSolver::unify(const TypeID dst, const TypeID src, const AST::Node& sourceNode) {
    nodes_[src].parent_ = dst;
    nodes_[dst].setSize_ += nodes_[src].setSize_;

    Type& dstType = typeManager_.getType(dst);
    const Type& srcType = typeManager_.getType(src);

    // The following modifies dstType only, but it is fine since src will never be root
    // again, so we don't care about its type anymore

    if (dstType.isUnknownKind() || srcType.isUnknownKind())
        return dstType.mergeWith(srcType, typeManager_);

    if (dstType.kind() != srcType.kind()) return false;

    switch (dstType.kind()) {
        case TypeKind::UNKNOWN:
            std::unreachable();
        case TypeKind::PRIMITIVE:
            return dstType.mergeWith(srcType, typeManager_);
        case TypeKind::ARRAY: {
            if (!dstType.matches(srcType, typeManager_)) return false;
            typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(
                dstType.arrayElementTypeId(), srcType.arrayElementTypeId(), sourceNode);
            return true;
        }
    }
    std::unreachable();
}

void TypeSolver::prepareUnionFind() {
    nodes_.clear();
    nodes_.resize(typeManager_.getTypeCount());
    for (TypeID i = 0; i < nodes_.size(); ++i) {
        nodes_[i] = {.parent_ = i, .setSize_ = 1};
    }
}

bool TypeSolver::solveEqualityConstraint(const EqualityConstraint& equalityConstraint) {
    // Uses a union-find algorithm to solve equality constraints
    assert(equalityConstraint.kind() == Constraint::Kind::EQUALITY);

    const TypeID a = equalityConstraint.a();
    const TypeID b = equalityConstraint.b();

    TypeID rootA = findRoot(a);
    TypeID rootB = findRoot(b);

    if (rootA == rootB) return true;

    if (nodes_[rootA].setSize_ < nodes_[rootB].setSize_) std::swap(rootA, rootB);

    if (!unify(rootA, rootB, equalityConstraint.sourceNode())) {
        // Types are not compatible
        const Type& aType = typeManager_.getType(rootA);
        const Type& bType = typeManager_.getType(rootB);

        diagnosticsEngine_.reportError(
            std::format("Type mismatch: cannot unify Type '{}' and '{}'",
                        aType.toString(typeManager_), bType.toString(typeManager_)),
            equalityConstraint.sourceNode().sourceStartIndex(),
            equalityConstraint.sourceNode().sourceEndIndex());
        diagnosticsEngine_.emitErrors();
        exit(EXIT_FAILURE);
    }

    return true;
}

bool TypeSolver::solveSubscriptConstraint(const SubscriptConstraint& subscriptConstraint) const {
    assert(subscriptConstraint.kind() == Constraint::Kind::SUBSCRIPT);

    const Type& type = typeManager_.getType(subscriptConstraint.container());

    if (type.kind() == TypeKind::ARRAY) {
        const TypeID expectedElementTypeID = type.arrayElementTypeId();
        const TypeID actualElementTypeID = subscriptConstraint.element();

        typeManager_.getTypeSolver().addConstraint(std::make_unique<EqualityConstraint>(
            expectedElementTypeID, actualElementTypeID, subscriptConstraint.sourceNode()));

        return true;
    }

    return false;
}

bool TypeSolver::solveHasTraitConstraint(const HasTraitConstraint& hasTraitConstraint) const {
    assert(hasTraitConstraint.kind() == Constraint::Kind::HAS_TRAIT);

    const Type& type = typeManager_.getType(hasTraitConstraint.type());
    const Trait& trait = hasTraitConstraint.trait();

    if (type.isUnknownKind()) return false;

    if (!type.hasTrait(trait)) {
        diagnosticsEngine_.reportError(
            std::format("Type '{}' does not implement the trait '{}'", type.toString(typeManager_),
                        traitToString(trait)),
            hasTraitConstraint.sourceNode().sourceStartIndex(),
            hasTraitConstraint.sourceNode().sourceEndIndex());
        diagnosticsEngine_.emitErrors();
        exit(EXIT_FAILURE);
    }

    return true;
}

bool TypeSolver::solveStorableConstraint(const StorableConstraint& storableConstraint) const {
    assert(storableConstraint.kind() == Constraint::Kind::STORABLE);

    const Type& type = typeManager_.getType(storableConstraint.type());

    if (type.kind() == TypeKind::PRIMITIVE) {
        if (type.primitive() == Primitive::Kind::VOID) {
            diagnosticsEngine_.reportError("Type 'void' is not storable",
                                           storableConstraint.sourceNode().sourceStartIndex(),
                                           storableConstraint.sourceNode().sourceEndIndex());
            diagnosticsEngine_.emitErrors();
            exit(EXIT_FAILURE);
        }
        return true;
    }

    if (type.kind() == TypeKind::ARRAY) {
        const TypeID elementTypeID = type.arrayElementTypeId();
        typeManager_.getTypeSolver().addConstraint(
            std::make_unique<StorableConstraint>(elementTypeID, storableConstraint.sourceNode()));
        return true;
    }

    return false;
}

void TypeSolver::solve() {
    prepareUnionFind();

    std::vector<std::unique_ptr<Constraint>> nextConstraints;

    while (!pendingConstraints_.empty()) {
        for (size_t i = 0; i < pendingConstraints_.size(); ++i) {
            const Constraint& constraint = *pendingConstraints_[i];

            bool solved = false;
            switch (constraint.kind()) {
                case Constraint::Kind::EQUALITY: {
                    auto& equalityConstraint = static_cast<const EqualityConstraint&>(constraint);
                    solved = solveEqualityConstraint(equalityConstraint);
                    break;
                }
                case Constraint::Kind::SUBSCRIPT: {
                    const auto& subscriptConstraint =
                        static_cast<const SubscriptConstraint&>(constraint);
                    solved = solveSubscriptConstraint(subscriptConstraint);
                    break;
                }
                case Constraint::Kind::HAS_TRAIT: {
                    const auto& hasTraitConstraint =
                        static_cast<const HasTraitConstraint&>(constraint);
                    solved = solveHasTraitConstraint(hasTraitConstraint);
                    break;
                }
                case Constraint::Kind::STORABLE: {
                    const auto& storableConstraint =
                        static_cast<const StorableConstraint&>(constraint);
                    solved = solveStorableConstraint(storableConstraint);
                    break;
                }
            }

            if (!solved) {
                nextConstraints.push_back(std::move(pendingConstraints_[i]));
            }
        }

        for (TypeID node = 0; node < nodes_.size(); ++node) {
            const TypeID root = findRoot(node);
            if (root == node) continue;

            typeManager_.linkTypes(root, node);
        }

        pendingConstraints_.swap(nextConstraints);
        nextConstraints.clear();
    }
}