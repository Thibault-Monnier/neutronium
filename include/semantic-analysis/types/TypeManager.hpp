#pragma once

#include <vector>

#include "Type.hpp"
#include "TypeSolver.hpp"

/**
 * @class TypeManager
 * @brief Manages a collection of Type objects.
 *
 * The TypeManager class provides functionality to manage a collection
 * of Type objects and gives each type a unique TypeID for easy reference.
 *
 * This class is specific to each translation unit and is not shared globally.
 */

class TypeManager {
   public:
    TypeManager() = default;

    /**
     * @brief Registers a new type in the type manager.
     *
     * This method copies the provided type object and stores it.
     * It returns a unique TypeID that can be used to reference the type later.
     *
     * @param type The type object to be registered in the type manager.
     * @return The unique TypeID assigned to the newly registered type.
     */
    [[nodiscard]] TypeID createType(const Type& type) {
        types_.push_back(std::make_unique<Type>(type));
        return static_cast<TypeID>(types_.size() - 1);
    }

    /**
     * @brief Retrieves a reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID
     * from the managed collection of types.
     *
     * @param id The unique TypeID of the type to retrieve.
     * @return A reference to the Type object associated with the provided TypeID.
     */
    [[nodiscard]] Type& getType(const TypeID id) { return *types_.at(id); }

    /**
     * @brief Retrieves a reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID
     * from the managed collection of types.
     *
     * @param id The unique TypeID of the type to retrieve.
     * @return A constant reference to the Type object associated with the provided TypeID.
     */
    [[nodiscard]] const Type& getType(const TypeID id) const { return *types_.at(id); }

    /**
     * @brief Provides access to the TypeSolver instance within the TypeManager.
     *
     * @return A reference to the TypeSolver managed by the TypeManager.
     */
    [[nodiscard]] TypeSolver& getTypeSolver() {
        return typeSolver_;
    }

   private:
    std::vector<std::unique_ptr<Type>> types_;
    TypeSolver typeSolver_;
};
