#pragma once

#include <deque>
#include <list>
#include <vector>

#include "Constraint.hpp"
#include "diagnostics_engine.hpp"

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
        : typeManager_(typeManager), diagnosticsEngine_(diagnosticsEngine) {};

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
    void addConstraint(Args&&... args) {
        static_assert(std::is_base_of_v<Constraint, ConstraintT>,
                      "ConstraintT must derive from Constraint");
        addConstraint(std::make_unique<ConstraintT>(std::forward<Args>(args)...));
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

   private:
    std::vector<std::unique_ptr<Constraint>> pendingConstraints_;

    TypeManager& typeManager_;
    DiagnosticsEngine& diagnosticsEngine_;

    struct Node {
        TypeID parent_;
        int setSize_;
    };

    std::vector<Node> nodes_;

    /**
     * @brief Registers a new type constraint to the collection of constraints.
     *
     * This method appends a new type constraint to the internal list of constraints. Type
     * constraints are used for ensuring type consistency and resolving type relationships during
     * semantic analysis.
     *
     * @param constraint A unique pointer to the Constraint object to be added.
     */
    void addConstraint(std::unique_ptr<Constraint> constraint) {
        pendingConstraints_.push_back(std::move(constraint));
    }

    [[nodiscard]] TypeID findRoot(TypeID x);
    [[nodiscard]] bool unify(TypeID dst, TypeID src, const AST::Node& sourceNode);
    void prepareUnionFind();
    bool solveEqualityConstraint(const EqualityConstraint& equalityConstraint);

    bool solveSubscriptConstraint(const SubscriptConstraint& subscriptConstraint) const;

    bool solveHasTraitConstraint(const HasTraitConstraint& hasTraitConstraint) const;
    bool solveStorableConstraint(const StorableConstraint& storableConstraint) const;
};
