#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>

#include "Primitive.hpp"
#include "PrimitiveTypeFamily.hpp"
#include "Trait.hpp"
#include "TypeID.hpp"

class TypeManager;

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
 * with various constructors and supports the concept of type families for primitive types².
 */
class Type {
   public:
    Type(const Primitive::Kind t, const PrimitiveTypeFamily* family, const TypeKind kind)
        : kind_(kind), primitive_(t), family_(family) {
        assert(family_->isInFamily(t) && "Type must be in its own family");
        initializeTraits();
    }

    Type(const TypeID elementTypeID, const std::size_t arrayLength)
        : kind_(TypeKind::ARRAY),
          primitive_(Primitive::Kind::VOID),
          arrayElementTypeID_(elementTypeID),
          arrayLength_(arrayLength) {
        initializeTraits();
    }

    Type(const Type& other) { copyFrom(other); }

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
        if (this != &other) copyFrom(other);
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
    static const Type& integerFamilyType() {
        static const Type instance(Primitive::Kind::INT, true);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing any family of primitive types.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the "any" family.
     *
     * @return A constant reference to the singleton instance of the "any" family type.
     */
    static const Type& anyFamilyType() {
        static const Type instance(Primitive::Kind::VOID, &AnyTypeFamily::getInstance(),
                                   TypeKind::UNKNOWN);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the void primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the void primitive type.
     *
     * @return A constant reference to the singleton instance of the void type.
     */
    static const Type& voidType() {
        static const Type instance(Primitive::Kind::VOID);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the boolean primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the boolean primitive type.
     *
     * @return A constant reference to the singleton instance of the boolean type.
     */
    static const Type& boolType() {
        static const Type instance(Primitive::Kind::BOOL);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the int8 primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the int8 primitive type.
     *
     * @return A constant reference to the singleton instance of the int8 type.
     */
    static const Type& int8Type() {
        static const Type instance(Primitive::Kind::INT8);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the int16 primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the int16 primitive type.
     *
     * @return A constant reference to the singleton instance of the int16 type.
     */
    static const Type& int16Type() {
        static const Type instance(Primitive::Kind::INT16);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the int32 primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the int32 primitive type.
     *
     * @return A constant reference to the singleton instance of the int32 type.
     */
    static const Type& int32Type() {
        static const Type instance(Primitive::Kind::INT32);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the int64 primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the int64 primitive type.
     *
     * @return A constant reference to the singleton instance of the int64 type.
     */
    static const Type& int64Type() {
        static const Type instance(Primitive::Kind::INT64);
        return instance;
    }

    /**
     * @brief Retrieves the predefined type representing the int primitive type.
     *
     * This method returns a constant reference to a singleton instance of the `Type` class
     * that represents the int primitive type.
     *
     * @return A constant reference to the singleton instance of the int type.
     */
    static const Type& intType() {
        static const Type instance(Primitive::Kind::INT);
        return instance;
    }

    /**
     * @brief Retrieves the family of the primitive type.
     *
     * @return A const pointer to the `PrimitiveTypeFamily` associated with this type.
     */
    [[nodiscard]] const PrimitiveTypeFamily* family() const { return family_; }

    [[nodiscard]] Primitive::Kind primitive() const { return primitive_; }

    /**
     * @brief Determines if the type represents a void primitive type.
     *
     * @return True if the type is a primitive type of kind `VOID`, false otherwise.
     */
    [[nodiscard]] bool isVoid() const {
        return kind_ == TypeKind::PRIMITIVE && primitive_ == Primitive::Kind::VOID;
    }

    /**
     * @brief Determines if the type is of kind `UNKNOWN`.
     *
     * @return True if the type is of kind `UNKNOWN`, false otherwise.
     */
    [[nodiscard]] bool isUnknownKind() const { return kind_ == TypeKind::UNKNOWN; }

    [[nodiscard]] bool isArray() const { return kind_ == TypeKind::ARRAY; }

    [[nodiscard]] bool isPrimitive() const { return kind_ == TypeKind::PRIMITIVE; }

    [[nodiscard]] int sizeBits(const TypeManager& typeManager) const;

    /**
     * @brief Converts the type information into a string representation.
     *
     * This method generates a string description of the type, including its kind
     * and relevant details. This is useful for debugging and logging purposes.
     *
     * @return A string that represents the type.
     */
    [[nodiscard]] std::string toString(const TypeManager& typeManager) const;

    [[nodiscard]] TypeKind kind() const { return kind_; }

    /**
     * @brief Retrieves the type identifier of the element type for an array type.
     *
     * This method returns the `TypeID` of the element type if the type
     * is of kind `ARRAY`. If the type is not an array, the behavior is undefined.
     *
     * @return The `TypeID` of the array's element type.
     */
    [[nodiscard]] TypeID arrayElementTypeId() const;

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
     * @param typeManager
     * @return True if the types were successfully merged; false otherwise.
     */
    [[nodiscard]] bool mergeWith(const Type& other, const TypeManager& typeManager);

    /**
     * @brief Checks whether this type matches another type.
     *
     * This method compares the current type to another type and determines if they match.
     * Two types are considered to match if their primitive kinds and families are compatible
     * (together), and all of their other properties are equal.
     *
     * @param other The type to compare with the current instance.
     * @param typeManager The type manager used for type comparisons.
     * @return True if the types are compatible; false otherwise.
     */
    [[nodiscard]] bool matches(const Type& other, const TypeManager& typeManager) const;

    /**
     * @brief Checks if a specific trait is present in the list of traits.
     *
     * This method iterates over the internal collection of traits and determines
     * whether the specified trait exists within the collection.
     *
     * @param trait The trait to be checked for existence in the list of traits.
     * @return True if the specified trait is present, false otherwise.
     */
    [[nodiscard]] bool hasTrait(const Trait trait) const {
        return (traits_ & static_cast<uint16_t>(trait)) != 0;
    }

   private:
    explicit Type(const Primitive::Kind t, const bool inFamily = false)
        : kind_(TypeKind::PRIMITIVE), primitive_(t) {
        if (inFamily) {
            family_ = PrimitiveTypeFamily::familyForType(t);
            assert(family_->isInFamily(t) && "Type must be in its own family");
        }
        initializeTraits();
    }

    TypeKind kind_;
    Primitive::Kind primitive_;

    uint16_t traits_;

    TypeID arrayElementTypeID_{0};

    const PrimitiveTypeFamily* family_ = &NoTypeFamily::getInstance();
    std::size_t arrayLength_{0};

    /**
     * @brief Initializes the traits of the type based on its kind and primitive kind.
     *
     * This method sets the `traits_` member variable according to the `kind_` and the `primitive_`
     * of the type. It assigns appropriate default traits for primitive Type and array Type, while
     * clearing traits for unknown Type.
     */
    void initializeTraits() {
        switch (kind_) {
            case TypeKind::PRIMITIVE:
                traits_ = Primitive::defaultTraits(primitive_);
                break;
            case TypeKind::ARRAY:
                traits_ = Trait::EQ | Trait::SUBSCRIPT;
                break;
            case TypeKind::UNKNOWN:
                traits_ = 0;
                break;
        }
    }

    // IMPORTANT NOTE:
    // All members must be copied here when adding new ones.
    void copyFrom(const Type& other) {
        kind_ = other.kind_;
        family_ = other.family_;
        primitive_ = other.primitive_;
        arrayElementTypeID_ = other.arrayElementTypeID_;
        arrayLength_ = other.arrayLength_;
        traits_ = other.traits_;
    }
};
