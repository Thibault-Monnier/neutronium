#pragma once

#include <concepts>
#include <type_traits>
#include <utility>
#include <vector>

#include "Constraint.hpp"
#include "ast/AST.hpp"
#include "diagnostics/DiagnosticsEngine.hpp"
#include "lib/PolymorphicArenaAllocator.hpp"
#include "type/Trait.hpp"
#include "type/Type.hpp"
#include "type/TypeID.hpp"

class TypeManager;

/**
 * @class TypeSolver
 * @brief A class responsible for managing and resolving type constraints.
 *
 * The TypeSolver is used to collect all type constraints of a translation unit and find a solution
 * for every type. It handles errors related to type constraints and ensures type consistency.
 */

class TypeSolver {
   public:
    TypeSolver(TypeManager& typeManager, DiagnosticsEngine& diagnosticsEngine)
        : typeManager_(typeManager), diagnosticsEngine_(diagnosticsEngine) {}

    /**
     * @brief Registers a new type constraint to the collection of constraints.
     *
     * This overload constructs the constraint in place, forwarding any arguments required
     * by the constraint's constructor.
     *
     * @tparam ConstraintT The specific type of constraint to be added, which must derive from
     * `Constraint`.
     * @param args Arguments forwarded to the constructor of the constraint.
     */
    template <typename ConstraintT, typename... Args>
        requires std::derived_from<ConstraintT, Constraint>
    void addConstraint(Args&&... args) {
        addConstraint(ConstraintT(std::forward<Args>(args)...));
    }

    /**
     * @brief Solves all registered type constraints to ensure type consistency.
     *
     * The solve method resolves all collected type constraints, including equality constraints and
     * trait requirements. It ensures that all types and their relationships satisfy the constraints
     * imposed during semantic analysis. This method internally uses the union-find algorithm
     * for equality constraints and processes trait requirements to finalize type resolution.
     */
    void solve();

    /**
     * @brief Prepares the TypeSolver for receiving constraints.
     *
     * This method initializes and reserves necessary data structures to efficiently handle
     * incoming type constraints.
     */
    void prepareForConstraints();

   private:
    neutro::PolymorphicArenaAllocator constraintArena_;
    std::vector<Constraint*> pendingConstraints_;

    TypeManager& typeManager_;
    DiagnosticsEngine& diagnosticsEngine_;

    struct Node {
        TypeID parent_;
        int setSize_;
    };

    std::vector<Node> nodes_;

    // ------------------------------
    // --- Error handling methods ---
    // ------------------------------

    [[noreturn]] void equalityConstraintError(TypeID a, TypeID b,
                                              const AST::Node& sourceNode) const;
    [[noreturn]] void hasTraitConstraintError(const Type& type, Trait trait,
                                              const AST::Node& sourceNode) const;
    [[noreturn]] void storableConstraintError(const Type& type, const AST::Node& sourceNode) const;

    /**
     * @brief Adds a type constraint to the pending constraints list.
     *
     * This method takes a constraint object, allocates it in the internal arena allocator,
     * and appends a pointer to it in the list of pending constraints to be solved.
     *
     * @tparam ConstraintT The type of the constraint being added, which must derive from
     * `Constraint`.
     * @param constraint The constraint object to be added.
     */
    template <typename ConstraintT>
    void addConstraint(ConstraintT&& constraint) {
        Constraint* ptr = constraintArena_.insert(std::forward<ConstraintT>(constraint));
        pendingConstraints_.push_back(ptr);
    }

    [[nodiscard]] TypeID findRoot(TypeID x);
    [[nodiscard]] bool unify(TypeID dst, TypeID src, const AST::Node& sourceNode);
    void prepareUnionFind();
    std::true_type solveEqualityConstraint(const EqualityConstraint& equalityConstraint);

    [[nodiscard]] bool solveSubscriptConstraint(const SubscriptConstraint& subscriptConstraint);
    [[nodiscard]] bool solveHasTraitConstraint(const HasTraitConstraint& hasTraitConstraint) const;
    [[nodiscard]] bool solveStorableConstraint(const StorableConstraint& storableConstraint);
};
