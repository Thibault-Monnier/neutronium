#pragma once

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
     * This method appends a new type constraint to the internal list of constraints maintained
     * by the TypeSolver. Type constraints are used for ensuring type consistency and resolving
     * type relationships during semantic analysis.
     *
     * @param constraint A unique pointer to the Constraint object to be added.
     */
    void addConstraint(std::unique_ptr<Constraint> constraint) {
        constraints_.push_back(std::move(constraint));
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
    std::vector<std::unique_ptr<Constraint>> constraints_;

    TypeManager& typeManager_;

    DiagnosticsEngine& diagnosticsEngine_;

    struct Node {
        TypeID parent_;
        int setSize_;
    };

    std::vector<Node> nodes_;

    [[nodiscard]] TypeID findRoot(TypeID x);
    [[nodiscard]] bool unify(TypeID dst, TypeID src);
    void solveEqualityConstraints();

    void solveHasTraitConstraints() const;
};
