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

    [[nodiscard]] TypeID createType(const Type& type) {
        types_.push_back(std::make_unique<Type>(type));
        return static_cast<TypeID>(types_.size() - 1);
    }

    [[nodiscard]] Type& getType(const TypeID id) { return *types_.at(id); }

    [[nodiscard]] const Type& getType(const TypeID id) const { return *types_.at(id); }

    [[nodiscard]] TypeSolver& getTypeSolver() { return typeSolver_; }

   private:
    std::vector<std::unique_ptr<Type>> types_;
    TypeSolver typeSolver_;
};
