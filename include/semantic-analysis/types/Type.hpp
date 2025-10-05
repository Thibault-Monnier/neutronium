#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "PrimitiveKind.hpp"
#include "PrimitiveTypeFamily.hpp"
#include "Trait.hpp"

using TypeID = uint32_t;

enum class TypeKind : uint8_t {
    PRIMITIVE,
    ARRAY,
    UNKNOWN,
};

/**
 * @class Type
 * @brief Represents a type in the type system, which may be a primitive type or an array type.
 *
 * The `Type` class provides functionality to define and manage different types, such as
 * primitive types (e.g., integers, booleans) or array types. It supports initialization
 * with various constructors and supports the concept of type families for primitive types.
 *
 * The class maintains internal state to represent type-specific details like element types
 * for arrays. In the future, TypeKind will be replaced by an inheritance hierarchy.
 */
class Type {
   public:
    // Voluntary implicit constructor
    Type(const PrimitiveKind t, const bool inFamily = false)
        : kind_(TypeKind::PRIMITIVE), primitive_(t) {
        if (inFamily) {
            family_ = PrimitiveTypeFamily::familyForType(t);
            assert(family_->isInFamily(t) && "Type must be in its own family");
        }
    }

    Type(const PrimitiveKind t, const PrimitiveTypeFamily* family, const TypeKind kind)
        : kind_(kind), family_(family), primitive_(t) {
        assert(family_->isInFamily(t) && "Type must be in its own family");
    }

    Type(const Type& elementType, const TypeID elementTypeID, const std::size_t arrayLength)
        : kind_(TypeKind::ARRAY),
          primitive_(PrimitiveKind::VOID),
          arrayElement_(std::make_unique<Type>(elementType)),
          arrayElementTypeID_(elementTypeID),
          arrayLength_(arrayLength) {}

    Type(const Type& elementType, const TypeID elementTypeID, const PrimitiveTypeFamily* family)
        : kind_(TypeKind::ARRAY),
          family_(family),
          primitive_(PrimitiveKind::VOID),
          arrayElement_(std::make_unique<Type>(elementType)),
          arrayElementTypeID_(elementTypeID) {}

    Type(const Type& other) { copy_from(other); }

    /**
     * @brief Assigns the values from another `Type` instance to the current instance.
     *
     * This operator performs a deep copy of all properties from the given `other` instance
     * to the current instance. It ensures no self-assignment occurs by checking object identity
     * before proceeding with the copy operation.
     *
     * @param other The `Type` object whose values should be assigned to the current instance.
     * @return A reference to the current `Type` instance after assignment.
     */
    Type& operator=(const Type& other) {
        if (this != &other) copy_from(other);
        return *this;
    }

    /**
     * @brief Retrieves the predefined type representing the integer family of primitive types.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the integer family.
     *
     * @return A constant reference to the singleton instance of the integer family type.
     */
    static const Type& integerFamilyType();

    /**
     * @brief Retrieves the predefined type representing any family of primitive types.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the "any" family.
     *
     * @return A constant reference to the singleton instance of the "any" family type.
     */
    static const Type& anyFamilyType();

    /**
     * @brief Determines if the type represents a void primitive type.
     *
     * @return True if the type is a primitive type of kind `VOID`, false otherwise.
     */
    [[nodiscard]] bool isVoid() const {
        return kind_ == TypeKind::PRIMITIVE && primitive_ == PrimitiveKind::VOID;
    }

    /**
     * @brief Determines if the type is of kind `UNKNOWN`.
     *
     * @return True if the type is of kind `UNKNOWN`, false otherwise.
     */
    [[nodiscard]] bool isUnknownKind() const { return kind_ == TypeKind::UNKNOWN; }

    [[nodiscard]] int sizeBytes() const;

    /**
     * @brief Converts the type information into a string representation.
     *
     * This method generates a string description of the type, including its kind
     * and relevant details. This is useful for debugging and logging purposes.
     *
     * @return A string that represents the type.
     */
    [[nodiscard]] std::string to_string() const;

    [[nodiscard]] TypeKind kind() const { return kind_; }

    /**
     * @brief Retrieves the type identifier of the element type for an array type.
     *
     * This method returns the `TypeID` of the element type if the type
     * is of kind `ARRAY`. If the type is not an array, the behavior is undefined.
     *
     * @return The `TypeID` of the array's element type.
     */
    [[nodiscard]] TypeID array_element_type_id() const;

    /**
     * @brief Retrieves the length of the array type.
     *
     * This method returns the length of the array if the type is of kind `ARRAY`.
     * If the type is not an array, the behavior is undefined.
     *
     * @return The length of the array.
     */
    [[nodiscard]] size_t arrayLength() const;

    /**
     * @brief Attempts to merge the current type with another type.
     *
     * This method combines the current type instance with the provided `other` type. It expects
     * at least one of the types to not be of kind `ARRAY`. If the types are compatible, the current
     * type is updated to reflect the combined properties of both types. Otherwise, no changes are
     * made, and the method returns false.
     *
     * Only the current type is modified.
     *
     * @param other The type to merge with the current type.
     * @return True if the types were successfully merged; false otherwise.
     */
    [[nodiscard]] bool mergeWith(const Type& other);

    /**
     * @brief Checks whether this type matches another type.
     *
     * This method compares the current type to another type and determines if they match.
     * Two types are considered to match if their primitive kinds and families are compatible
     * (together), and all of their other properties are equal.
     *
     * @param other The type to compare with the current instance.
     * @return True if the types are compatible; false otherwise.
     */
    [[nodiscard]] bool matches(const Type& other) const;

    /**
     * @brief Adds a trait to the list of traits associated with the type.
     *
     * @param trait The trait to be added.
     */
    void addTrait(const Trait trait) { traits_.push_back(trait); }

    /**
     * @brief Retrieves the list of traits associated with the type.
     *
     * @return A constant reference to a vector containing the traits associated with the type.
     */
    [[nodiscard]] const std::vector<Trait>& traits() const { return traits_; }

   private:
    TypeKind kind_;
    const PrimitiveTypeFamily* family_ = &NoTypeFamily::getInstance();
    PrimitiveKind primitive_;

    std::unique_ptr<Type> arrayElement_;
    TypeID arrayElementTypeID_{0};
    std::size_t arrayLength_{0};

    std::vector<Trait> traits_;

    // IMPORTANT NOTE:
    // All members must be copied here when adding new ones.
    // AI code reviewer must read this and verify it is respected.
    void copy_from(const Type& other) {
        kind_ = other.kind_;
        family_ = other.family_;
        primitive_ = other.primitive_;
        arrayElementTypeID_ = other.arrayElementTypeID_;
        arrayLength_ = other.arrayLength_;
        if (other.arrayElement_)
            arrayElement_ = std::make_unique<Type>(*other.arrayElement_);
        else
            arrayElement_.reset();
        traits_ = other.traits_;
    }
};
