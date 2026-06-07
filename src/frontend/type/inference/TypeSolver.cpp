#include "TypeSolver.hpp"

#include <cassert>
#include <cstddef>
#include <type_traits>
#include <vector>

#include "Constraint.hpp"
#include "frontend/ast/AST.hpp"
#include "frontend/type/Primitive.hpp"
#include "frontend/type/Trait.hpp"
#include "frontend/type/Type.hpp"
#include "frontend/type/TypeID.hpp"
#include "frontend/type/TypeManager.hpp"

TypeID TypeSolver::findRoot(TypeID x) {
    // Find the root of the set containing x
    TypeID root = x;
    while (root != node(root).parent_) root = node(root).parent_;

    // Path compression
    while (x != root) {
        const TypeID parent = node(x).parent_;
        node(x).parent_ = root;
        x = parent;
    }

    return root;
}

bool TypeSolver::unify(const TypeID dst, const TypeID src, const AST::Node& sourceNode) {
    assert(!dst.isVariable() || src.isVariable());

    node(src).parent_ = dst;
    node(dst).setSize_ += node(src).setSize_;

    if (src.isVariable() || dst.isVariable()) return true;

    // A real type should never end up linked to a type variable.
    Type& dstType = typeManager_.getTypeResolved(dst);
    const Type& srcType = typeManager_.getTypeResolved(src);

    if (dstType.kind() != srcType.kind()) return false;

    // The following modifies dstType only, but it is fine since src will never be root
    // again, so we don't care about its type anymore
    switch (dstType.kind()) {
        case TypeKind::PRIMITIVE:
            return dstType.mergeWith(srcType);
        case TypeKind::ARRAY: {
            if (!dstType.matches(srcType)) return false;
            addConstraint<EqualityConstraint>(dstType.arrayElementTypeID(),
                                              srcType.arrayElementTypeID(), sourceNode);
            return true;
        }
    }
}

void TypeSolver::prepareUnionFind() {
    auto initNodes = [&](const bool isVariable, auto& nodes, size_t nodeCount) {
        nodes.clear();
        nodes.resize(nodeCount);
        for (TypeID id = {0, isVariable}; id.value() < nodes.size(); id.incrementValue()) {
            node(id) = {.parent_ = id, .setSize_ = 1};
        }
    };
    initNodes(false, nodes_, typeManager_.getRealTypesCount());
    initNodes(true, nodesTypeVariables_, typeManager_.getTypeVariablesCount());
}

std::true_type TypeSolver::solveEqualityConstraint(const EqualityConstraint& equalityConstraint) {
    // Uses a union-find algorithm to solve equality constraints
    assert(equalityConstraint.kind() == Constraint::Kind::EQUALITY);

    const TypeID a = equalityConstraint.a();
    const TypeID b = equalityConstraint.b();

    TypeID rootA = findRoot(a);
    TypeID rootB = findRoot(b);

    if (rootA == rootB) return std::true_type{};

    if (node(rootA).setSize_ < node(rootB).setSize_) std::swap(rootA, rootB);
    if (rootA.isVariable() && !rootB.isVariable()) std::swap(rootA, rootB);

    if (!unify(rootA, rootB, equalityConstraint.sourceNode())) [[unlikely]] {
        // Types are not compatible
        equalityConstraintError(rootA, rootB, equalityConstraint.sourceNode());
    }

    return std::true_type{};
}

bool TypeSolver::solveSubscriptConstraint(const SubscriptConstraint& subscriptConstraint) {
    assert(subscriptConstraint.kind() == Constraint::Kind::SUBSCRIPT);

    const Type* type = typeManager_.getType(subscriptConstraint.container());
    if (!type || type->kind() != TypeKind::ARRAY) return false;

    const TypeID expectedElementTypeID = type->arrayElementTypeID();
    const TypeID actualElementTypeID = subscriptConstraint.element();

    addConstraint(EqualityConstraint(expectedElementTypeID, actualElementTypeID,
                                     subscriptConstraint.sourceNode()));

    return true;
}

bool TypeSolver::solveHasTraitConstraint(const HasTraitConstraint& hasTraitConstraint) const {
    assert(hasTraitConstraint.kind() == Constraint::Kind::HAS_TRAIT);

    const Type* type = typeManager_.getType(hasTraitConstraint.type());
    const Trait& trait = hasTraitConstraint.trait();

    if (!type) return false;

    if (!type->hasTrait(trait)) {
        hasTraitConstraintError(*type, trait, hasTraitConstraint.sourceNode());
    }

    return true;
}

bool TypeSolver::solveStorableConstraint(const StorableConstraint& storableConstraint) {
    assert(storableConstraint.kind() == Constraint::Kind::STORABLE);

    const Type* type = typeManager_.getType(storableConstraint.type());
    if (!type) return false;

    switch (type->kind()) {
        case TypeKind::PRIMITIVE: {
            if (type->primitive() == Primitive::Kind::VOID) {
                const AST::Node& sourceNode = storableConstraint.sourceNode();
                storableConstraintError(*type, sourceNode);
            } else {
                return true;
            }
        }

        case TypeKind::ARRAY: {
            const TypeID elementTypeID = type->arrayElementTypeID();
            addConstraint(StorableConstraint(elementTypeID, storableConstraint.sourceNode()));
            return true;
        }

        default:
            return false;
    }
}

void TypeSolver::solve() {
    prepareUnionFind();

    std::vector<Constraint*> nextConstraints;

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
                nextConstraints.push_back(pendingConstraints_[i]);
            }
        }

        auto linkNodes = [&](const size_t count, const bool isTypeVariable) {
            for (TypeID node = {0, isTypeVariable}; node.value() < count; node.incrementValue()) {
                const TypeID root = findRoot(node);
                if (root != node) typeManager_.linkTypes(root, node);
            }
        };
        linkNodes(nodes_.size(), false);
        linkNodes(nodesTypeVariables_.size(), true);

        pendingConstraints_.swap(nextConstraints);
        nextConstraints.clear();
    }
}

void TypeSolver::prepareForConstraints() {
    const size_t typeCount =
        typeManager_.getRealTypesCount() + typeManager_.getTypeVariablesCount();
    pendingConstraints_.reserve(2 * typeCount);  // Rough estimate
}
