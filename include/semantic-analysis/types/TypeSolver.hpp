#pragma once

#include <vector>

#include "Constraint.hpp"

/**
 * @class TypeSolver
 * @brief A class responsible for managing and resolving type constraints.
 *
 * The TypeSolver is used to collect all type constraints of a translation unit and find a solution
 * for every type. It handles errors related to type constraints and ensures type consistency.
 */

class TypeSolver {
   public:
    TypeSolver() = default;

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

   private:
    std::vector<std::unique_ptr<Constraint>> constraints_;
};
