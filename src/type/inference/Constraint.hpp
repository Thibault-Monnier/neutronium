#pragma once

#include "../Trait.hpp"
#include "../TypeID.hpp"
#include "ast/AST.hpp"

/**
 * @brief Represents a type constraint for the TypeSolver.
 *
 * The Constraint class serves as a base class for different kinds of constraints that
 * enforce specific rules or relationships between types. It also holds a reference to the
 * source location in the AST where the constraint comes from for error reporting.
 */
class Constraint {
   public:
    enum class Kind : uint8_t { EQUALITY, SUBSCRIPT, HAS_TRAIT, STORABLE };

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
 * @brief Represents a subscript constraint for a container type.
 *
 * The SubscriptConstraint class defines a constraint that asserts a specific type
 * must have the provided element type.
 */
class SubscriptConstraint final : public Constraint {
   public:
    SubscriptConstraint(const TypeID container, const TypeID element, const AST::Node& sourceNode)
        : Constraint(sourceNode), container_(container), element_(element) {}

    [[nodiscard]] Kind kind() const override { return Kind::SUBSCRIPT; }

    [[nodiscard]] TypeID container() const { return container_; }
    [[nodiscard]] TypeID element() const { return element_; }

   private:
    const TypeID container_;
    const TypeID element_;
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

/**
 * @brief Represents a storable constraint for a type.
 *
 * The StorableConstraint class defines a constraint that asserts a specific type
 * is storable (i.e., can be stored in variables or data structures).
 */
class StorableConstraint final : public Constraint {
   public:
    StorableConstraint(const TypeID type, const AST::Node& sourceNode)
        : Constraint(sourceNode), type_(type) {}

    [[nodiscard]] Kind kind() const override { return Kind::STORABLE; }

    [[nodiscard]] TypeID type() const { return type_; }

   private:
    const TypeID type_;
};