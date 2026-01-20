#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>
#include <vector>

#include "Type.hpp"

/// An arena allocator for holding Type data. This allows for fast bulk allocation and deallocation.
class TypeArena {
   public:
    TypeArena() = default;

    TypeArena(const TypeArena&) = delete;
    TypeArena& operator=(const TypeArena&) = delete;

    TypeArena(TypeArena&&) = delete;
    TypeArena& operator=(TypeArena&&) = delete;

    ~TypeArena() {
        static_assert(std::is_trivially_destructible_v<Type>);
        for (void* block : blocks_) {
            ::operator delete(block, static_cast<std::align_val_t>(ALIGNMENT));
        }
    }

    /** Push a new Type into the arena.
     *
     * @param type The Type to push.
     */
    void push(Type&& type) {
        void* pos = reinterpret_cast<void*>(allocate());
        std::construct_at(static_cast<Type*>(pos), std::move(type));
    }

    /** Access a Type in the arena by index.
     *
     * @param index The index of the Type to access.
     * @return A reference to the Type at the given index.
     */
    [[nodiscard]] Type& at(const size_t index) const {
        const size_t block = index / BLOCK_SIZE_ELEMS;
        const size_t idx = index % BLOCK_SIZE_ELEMS;
        return blocks_[block][idx];
    }

    /** Get the number of Types in the arena.
     *
     * @return The number of Types in the arena.
     */
    [[nodiscard]] size_t count() const { return count_; }

   private:
    uintptr_t allocate(const size_t size = sizeof(Type), const size_t alignment = alignof(Type)) {
        assert(alignment == ALIGNMENT && "Alignments don't match");

        uintptr_t pos = currentBlockPos_;
        uintptr_t newPos = pos + size;

        if (newPos > currentBlockEnd_) {
            allocateBlock();

            pos = currentBlockPos_;
            newPos = pos + size;
        }

        currentBlockPos_ = newPos;
        count_++;
        return pos;
    }

    void allocateBlock() {
        void* block = ::operator new(BLOCK_SIZE_BYTES, static_cast<std::align_val_t>(ALIGNMENT));
        blocks_.push_back(static_cast<Type*>(block));
        currentBlockPos_ = reinterpret_cast<uintptr_t>(block);
        currentBlockEnd_ = reinterpret_cast<uintptr_t>(block) + BLOCK_SIZE_BYTES;
    }

   private:
    static constexpr size_t ALIGNMENT = alignof(Type);

    static constexpr size_t BLOCK_SIZE_BYTES = 1 << 20;
    static constexpr size_t BLOCK_SIZE_ELEMS = BLOCK_SIZE_BYTES / sizeof(Type);

    std::vector<Type*> blocks_;
    uintptr_t currentBlockPos_ = 0;
    uintptr_t currentBlockEnd_ = 0;

    size_t count_ = 0;
};
