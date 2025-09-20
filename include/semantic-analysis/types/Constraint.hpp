#pragma once

#include <semantic-analysis/types/Type.hpp>

#include "Trait.hpp"
#include "parsing/ast.hpp"

class Constraint {
   public:
    enum class Kind : uint8_t { EQUALITY, HAS_TRAIT };

    Constraint(const Kind k, const AST::Node& sourceNode) : kind_(k), sourceNode_(sourceNode) {}

   private:
    const Kind kind_;

    const AST::Node& sourceNode_;
};

class EqualityConstraint : public Constraint {
   public:
    EqualityConstraint(const TypeID a, const TypeID b, const AST::Node& sourceNode)
        : Constraint(Kind::EQUALITY, sourceNode), a_(a), b_(b) {}

   private:
    const TypeID a_;
    const TypeID b_;
};

class HasTraitConstraint : public Constraint {
   public:
    HasTraitConstraint(const TypeID type, const Trait trait, const AST::Node& sourceNode)
        : Constraint(Kind::HAS_TRAIT, sourceNode), type_(type), trait_(trait) {}

   private:
    const TypeID type_;
    const Trait trait_;
};