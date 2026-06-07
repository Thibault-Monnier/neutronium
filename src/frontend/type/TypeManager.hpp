#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>
#include <vector>

#include "Type.hpp"
#include "frontend/diagnostics/DiagnosticsEngine.hpp"
#include "frontend/type/TypeID.hpp"
#include "inference/TypeSolver.hpp"
#include "lib/SpecializedArenaAllocator.hpp"

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

    /** Clears all registered types and resets the state. */
    void clear() {
        typeArena_.clear();
        typeSolver_.clear();
        linkingTable_ = std::vector<TypeID>();
        linkingTableTypeVariables_ = std::vector<TypeID>();
    }

    /**
     * @brief Registers a new type in the type manager.
     *
     * This method copies the provided type object and stores it.
     * It returns a unique TypeID that can be used to reference the type later.
     *
     * @param type The type object to be registered in the type manager.
     * @return The unique TypeID assigned to the newly registered type.
     */
    [[nodiscard]] TypeID createType(Type type) {
        const TypeID id = {typeArena_.insert(std::move(type)), false};
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
     * @brief Creates a new type variable and registers it in the type manager.
     *
     * A type variable is a type that has no known properties. Its only purpose is to be merged with
     * other types. This is faster than creating a normal initialized type by saving memory.
     *
     * @return The unique TypeID assigned to the newly created type variable.
     */
    [[nodiscard]] TypeID createTypeVariable() {
        const TypeID id = {static_cast<uint32_t>(linkingTableTypeVariables_.size()), true};
        linkingTableTypeVariables_.push_back(id);
        return id;
    }

    /**
     * @brief Retrieves a pointer to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID from the managed
     * collection of types. If it is a type variable, it returns nullptr.
     */
    [[nodiscard]] const Type* getType(const TypeID id) const {
        const TypeID resolvedID = linkedTypeID(id);
        if (resolvedID.isVariable()) {
            return nullptr;
        }
        return &typeArena_.at(resolvedID.value());
    }

    /**
     * @brief Retrieves a pointer to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID
     * from the managed collection of types.
     */
    [[nodiscard]] Type* getType(const TypeID id) {
        return const_cast<Type*>(std::as_const(*this).getType(id));
    }

    /**
     * @brief Retrieves a reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID from the managed
     * collection of types. It asserts that the type isn't a type variable.
     */
    [[nodiscard]] const Type& getTypeResolved(const TypeID id) const {
        const Type* type = getType(id);
        assert(type != nullptr);
        return *type;
    }

    /**
     * @brief Retrieves a constant reference to a previously registered type.
     *
     * This function fetches the type corresponding to the provided TypeID from the managed
     * collection of types. It asserts that the type isn't a type variable.
     */
    [[nodiscard]] Type& getTypeResolved(const TypeID id) {
        return const_cast<Type&>(std::as_const(*this).getTypeResolved(id));
    }

    /**
     * @brief Retrieves the total number of non-variable types registered in the TypeManager.
     *
     * @warning This includes types that may have been linked to others via the linking table. It
     * does not reflect the number of unique types that result from the type-merging.
     *
     * @return The number of types registered in the TypeManager.
     */
    [[nodiscard]] size_t getRealTypesCount() const { return typeArena_.count(); }

    /**
     * @brief Retrieves the total number of type variables registered in the TypeManager.
     *
     * @return The number of type variables registered in the TypeManager.
     */
    [[nodiscard]] size_t getTypeVariablesCount() const { return linkingTableTypeVariables_.size(); }

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
    void linkTypes(const TypeID dst, const TypeID src) { linkedTypeID(src) = dst; }

   private:
    neutro::SpecializedArenaAllocator<Type> typeArena_;
    TypeSolver typeSolver_;

    /** @brief A mapping of TypeIDs to their linked TypeIDs.
     *
     * This vector adds a layer of indirection for TypeIDs, allowing one TypeID to reference
     * another. This is used to allow type equivalence, where modifying one type affects all linked
     * types. Initially, every TypeID maps to itself.
     */
    std::vector<TypeID> linkingTable_, linkingTableTypeVariables_;

    [[nodiscard]] const TypeID& linkedTypeID(const TypeID id) const {
        auto& linkingTable = id.isVariable() ? linkingTableTypeVariables_ : linkingTable_;
        assert(id.value() < linkingTable.size());
        return linkingTable[id.value()];
    }

    [[nodiscard]] TypeID& linkedTypeID(const TypeID id) {
        return const_cast<TypeID&>(std::as_const(*this).linkedTypeID(id));
    }
};
