#pragma once

#include <vector>

#include "Type.hpp"
#include "TypeArena.hpp"
#include "inference/TypeSolver.hpp"

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
    explicit TypeManager(DiagnosticsEngine& diagnosticsEngine)
        : typeSolver_(*this, diagnosticsEngine) {}

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
        const auto id = static_cast<TypeID>(typeArena_.size());
        typeArena_.push(std::make_unique<Type>(type));
        linkingTable_.push_back(id);
        return id;
    }

    /**
     * @brief Registers a new type in the type manager.
     *
     * This method constructs a new Type object in place using the provided arguments and stores it.
     * It returns a unique TypeID that can be used to reference the type later.
     *
     * @param args Arguments forwarded to the Type constructor.
     * @return The unique TypeID assigned to the newly registered type.
     */
    template <class... Args>
        requires(!std::conjunction_v<std::is_same<std::remove_cvref_t<Args>, Type>...>)
    [[nodiscard]] TypeID createType(Args&&... args) {
        return createType(Type(std::forward<Args>(args)...));
    }

    /**
     * @brief Retrieves a reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID
     * from the managed collection of Type.
     *
     * @param id The unique TypeID of the type to retrieve.
     * @return A constant reference to the Type object associated with the provided TypeID.
     */
    [[nodiscard]] const Type& getType(const TypeID id) const {
        const TypeID resolvedID = linkingTable_.at(id);
        return typeArena_.at(resolvedID);
    }

    /**
     * @brief Retrieves a reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID
     * from the managed collection of Type.
     *
     * @param id The unique TypeID of the type to retrieve.
     * @return A mutable reference to the Type object associated with the provided TypeID.
     */
    [[nodiscard]] Type& getType(const TypeID id) {
        return const_cast<Type&>(std::as_const(*this).getType(id));
    }

    /**
     * @brief Retrieves the total number of registered types.
     *
     * @warning This includes types that may have been linked to others via the linking table. It
     * does not reflect the number of unique types that result from the type-merging.
     *
     * @return The number of types registered in the TypeManager.
     */
    [[nodiscard]] size_t getTypeCount() const { return typeArena_.size(); }

    /**
     * @brief Provides access to the TypeSolver instance within the TypeManager.
     *
     * @return A reference to the TypeSolver managed by the TypeManager.
     */
    [[nodiscard]] TypeSolver& getTypeSolver() { return typeSolver_; }

    /**
     * @brief Add a type-equivalence entry to the linking table.
     *
     * Stores a mapping so that any lookup of @p src resolves to @p dst.
     *
     * @param dst TypeID that becomes the target of the mapping.
     * @param src TypeID that will be redirected to dst.
     *
     * @note If dst is later linked to another TypeID, src will NOT automatically update to point to
     * that new TypeID. It will be left invalid.
     */
    void linkTypes(const TypeID dst, const TypeID src) { linkingTable_[src] = dst; }

   private:
    TypeArena typeArena_;
    TypeSolver typeSolver_;

    /** @brief A mapping of TypeIDs to their linked TypeIDs.
     *
     * This vector adds a layer of indirection for TypeIDs, allowing one TypeID to reference
     * another. This is used to allow type equivalence, where modifying one type affects all linked
     * Type. Initially, every TypeID maps to itself.
     */
    std::vector<TypeID> linkingTable_;
};
