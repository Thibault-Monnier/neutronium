#pragma once

#include <vector>

struct Constraint {};

class TypeInferrer {
   public:
    TypeInferrer() = default;

    void addConstraint(const Constraint& constraint) { constraints_.push_back(constraint); }

   private:
    std::vector<Constraint> constraints_;
};
