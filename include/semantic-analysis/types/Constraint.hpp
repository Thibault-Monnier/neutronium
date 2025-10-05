#pragma once

#include <semantic-analysis/types/Type.hpp>

#include "Trait.hpp"
#include "parsing/ast.hpp"

/**
 * @brief Represents a type constraint for the TypeSolver.
 *
 * The Constraint class serves as a base class for different kinds of constraints that
 * enforce specific rules or relationships between types. It also holds a reference to the
 * source location in the AST where the constraint comes from for error reporting.
 */
class Constraint {
   public:
    enum class Kind : uint8_t { EQUALITY, HAS_TRAIT };

    virtual ~Constraint() = default;

    [[nodiscard]] virtual Kind kind() const = 0;
    [[nodiscard]] const AST::Node& sourceNode() const { return sourceNode_; }

   protected:
    explicit Constraint(const AST::Node& sourceNode) : sourceNode_(sourceNode) {}

   private:
    const AST::Node& sourceNode_;
};

/**
 * @brief Represents an equality constraint between two types.
 *
 * The EqualityConstraint class defines a constraint that asserts two types
 * must be strictly equal.
 */
class EqualityConstraint final : public Constraint {
   public:
    EqualityConstraint(const TypeID a, const TypeID b, const AST::Node& sourceNode)
        : Constraint(sourceNode), a_(a), b_(b) {}

    [[nodiscard]] Kind kind() const override { return Kind::EQUALITY; }

    [[nodiscard]] TypeID a() const { return a_; }
    [[nodiscard]] TypeID b() const { return b_; }

   private:
    const TypeID a_;
    const TypeID b_;
};

/**
 * @brief Represents a trait constraint for a type.
 *
 * The HasTraitConstraint class defines a constraint that asserts a specific type
 * must implement or possess a given trait.
 */
class HasTraitConstraint final : public Constraint {
   public:
    HasTraitConstraint(const TypeID type, const Trait trait, const AST::Node& sourceNode)
        : Constraint(sourceNode), type_(type), trait_(trait) {}

    [[nodiscard]] Kind kind() const override { return Kind::HAS_TRAIT; }

    [[nodiscard]] TypeID type() const { return type_; }
    [[nodiscard]] Trait trait() const { return trait_; }

   private:
    const TypeID type_;
    const Trait trait_;
};
