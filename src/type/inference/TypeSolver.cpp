#include "TypeSolver.hpp"

#include <cassert>
#include <cstdlib>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

#include "Constraint.hpp"
#include "ast/AST.hpp"
#include "type/Primitive.hpp"
#include "type/Trait.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"
#include "type/TypeManager.hpp"

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

    if (dstType.isUnknownKind() || srcType.isUnknownKind()) return dstType.mergeWith(srcType);

    if (dstType.kind() != srcType.kind()) return false;

    switch (dstType.kind()) {
        case TypeKind::PRIMITIVE:
            return dstType.mergeWith(srcType);
        case TypeKind::ARRAY: {
            if (!dstType.matches(srcType)) return false;
            typeManager_.getTypeSolver().addConstraint<EqualityConstraint>(
                dstType.arrayElementTypeId(), srcType.arrayElementTypeId(), sourceNode);
            return true;
        }
        default:
            std::unreachable();
    }
}

void TypeSolver::prepareUnionFind() {
    nodes_.clear();
    nodes_.resize(typeManager_.getTypeCount());
    for (TypeID i = 0; i < nodes_.size(); ++i) {
        nodes_[i] = {.parent_ = i, .setSize_ = 1};
    }
}

std::true_type TypeSolver::solveEqualityConstraint(const EqualityConstraint& equalityConstraint) {
    // Uses a union-find algorithm to solve equality constraints
    assert(equalityConstraint.kind() == Constraint::Kind::EQUALITY);

    const TypeID a = equalityConstraint.a();
    const TypeID b = equalityConstraint.b();

    TypeID rootA = findRoot(a);
    TypeID rootB = findRoot(b);

    if (rootA == rootB) return std::true_type{};

    if (nodes_[rootA].setSize_ < nodes_[rootB].setSize_) std::swap(rootA, rootB);

    if (!unify(rootA, rootB, equalityConstraint.sourceNode())) [[unlikely]] {
        // Types are not compatible
        equalityConstraintError(rootA, rootB, equalityConstraint.sourceNode());
    }

    return std::true_type{};
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
        hasTraitConstraintError(type, trait, hasTraitConstraint.sourceNode());
    }

    return true;
}

bool TypeSolver::solveStorableConstraint(const StorableConstraint& storableConstraint) const {
    assert(storableConstraint.kind() == Constraint::Kind::STORABLE);

    const Type& type = typeManager_.getType(storableConstraint.type());

    switch (type.kind()) {
        case TypeKind::PRIMITIVE: {
            if (type.primitive() == Primitive::Kind::VOID) {
                const AST::Node& sourceNode = storableConstraint.sourceNode();
                storableConstraintError(type, sourceNode);
            } else {
                return true;
            }
        }

        case TypeKind::ARRAY: {
            const TypeID elementTypeID = type.arrayElementTypeId();
            typeManager_.getTypeSolver().addConstraint(std::make_unique<StorableConstraint>(
                elementTypeID, storableConstraint.sourceNode()));
            return true;
        }

        default:
            return false;
    }
}

void TypeSolver::solve() {
    prepareUnionFind();

    std::vector<std::unique_ptr<Constraint>> nextConstraints;

    while (!pendingConstraints_.empty()) {
        for (size_t i = 0; i < pendingConstraints_.size(); ++i) {
            const Constraint& constraint = *pendingConstraints_[i];

            bool solved = false;
            switch (constraint.kind()) {
                case Constraint::Kind::EQUALITY:
                    solved = solveEqualityConstraint(constraint.as<const EqualityConstraint>());
                    break;
                case Constraint::Kind::SUBSCRIPT:
                    solved = solveSubscriptConstraint(constraint.as<const SubscriptConstraint>());
                    break;
                case Constraint::Kind::HAS_TRAIT:
                    solved = solveHasTraitConstraint(constraint.as<const HasTraitConstraint>());
                    break;
                case Constraint::Kind::STORABLE:
                    solved = solveStorableConstraint(constraint.as<const StorableConstraint>());
                    break;
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