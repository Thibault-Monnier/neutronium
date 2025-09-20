#pragma once

#include <vector>

#include "Constraint.hpp"

class TypeSolver {
   public:
    TypeSolver() = default;

    void addConstraint(const Constraint& constraint) { constraints_.push_back(constraint); }

   private:
    std::vector<Constraint> constraints_;
};
