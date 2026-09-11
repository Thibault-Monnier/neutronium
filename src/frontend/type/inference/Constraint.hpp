#pragma once

#include <cstdint>

#include "../Trait.hpp"
#include "../TypeID.hpp"
#include "frontend/ast/AST.hpp"

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

    [[nodiscard]] Kind kind() const { return static_cast<Kind>(data_ & KIND_MASK); }

    [[nodiscard]] const AST::Node& sourceNode() const {
        return *reinterpret_cast<const AST::Node*>(data_ & PTR_MASK);
    }

    template <typename T>
    [[nodiscard]] T& as() {
        return *static_cast<T*>(this);
    }
    template <typename T>
    [[nodiscard]] T& as() const {
        return *static_cast<const T*>(this);
    }

   protected:
    explicit Constraint(const AST::Node& sourceNode, const Kind kind)
        : data_(reinterpret_cast<uintptr_t>(&sourceNode) |
                (static_cast<uintptr_t>(kind) & KIND_MASK)) {}
    ~Constraint() = default;

   private:
    uintptr_t data_;

    // Pointer is 8-byte aligned, so we don't care about the last 3 bits. We can use them to store
    // the kind of constraint.
    static constexpr uintptr_t KIND_MASK = 0b111;
    static constexpr uintptr_t PTR_MASK = ~KIND_MASK;
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
        : Constraint(sourceNode, Kind::EQUALITY), a_(a), b_(b) {}

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
        : Constraint(sourceNode, Kind::SUBSCRIPT), container_(container), element_(element) {}

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
        : Constraint(sourceNode, Kind::HAS_TRAIT), trait_(trait), type_(type) {}

    [[nodiscard]] TypeID type() const { return type_; }
    [[nodiscard]] Trait trait() const { return trait_; }

   private:
    const Trait trait_;
    const TypeID type_;
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
        : Constraint(sourceNode, Kind::STORABLE), type_(type) {}

    [[nodiscard]] TypeID type() const { return type_; }

   private:
    const TypeID type_;
};
