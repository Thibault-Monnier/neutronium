#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>
#include <vector>

namespace neutro {

/** Arena allocator for storing elements of a single trivially destructible type.
 *
 * Allocates elements in large blocks to reduce allocation overhead.
 * Does not call destructors on elements. Elements cannot be freed
 * individually; all memory is freed when the allocator is destroyed.
 *
 * @tparam T The type of the elements to store in the arena. Must be trivially destructible.
 */
template <typename T>
    requires std::is_trivially_destructible_v<T>
class SpecializedArenaAllocator {
   public:
    SpecializedArenaAllocator() = default;

    SpecializedArenaAllocator(const SpecializedArenaAllocator&) = delete;
    SpecializedArenaAllocator& operator=(const SpecializedArenaAllocator&) = delete;

    SpecializedArenaAllocator(SpecializedArenaAllocator&&) = delete;
    SpecializedArenaAllocator& operator=(SpecializedArenaAllocator&&) = delete;

    ~SpecializedArenaAllocator() {
        for (void* block : blocks_) {
            ::operator delete(block, static_cast<std::align_val_t>(ALIGNMENT));
        }
    }

    /** Push a new element into the arena.
     *
     * @param elem The element to push.
     */
    uint32_t push(T&& elem) {
        void* pos = reinterpret_cast<void*>(allocate());
        std::construct_at(static_cast<T*>(pos), std::move(elem));
        return static_cast<uint32_t>(count_ - 1);
    }

    /** Access an element in the arena by index.
     *
     * @param index The index of the element to access.
     * @return A reference to the element at the given index.
     */
    [[nodiscard]] T& at(const size_t index) const {
        assert(index < count_ && "Index out of bounds");
        const size_t block = index / BLOCK_SIZE_ELEMS;
        const size_t idx = index % BLOCK_SIZE_ELEMS;
        return blocks_[block][idx];
    }

    /** Get the number of elements in the arena.
     *
     * @return The number of elements in the arena.
     */
    [[nodiscard]] size_t count() const { return count_; }

   private:
    uintptr_t allocate() {
        uintptr_t pos = currentBlockPos_;
        uintptr_t newPos = pos + sizeof(T);

        if (newPos > currentBlockEnd_) {
            allocateBlock();

            pos = currentBlockPos_;
            newPos = pos + sizeof(T);
        }

        currentBlockPos_ = newPos;
        count_++;
        return pos;
    }

    void allocateBlock() {
        void* block = ::operator new(BLOCK_SIZE_BYTES, static_cast<std::align_val_t>(ALIGNMENT));
        blocks_.push_back(static_cast<T*>(block));
        currentBlockPos_ = reinterpret_cast<uintptr_t>(block);
        currentBlockEnd_ = reinterpret_cast<uintptr_t>(block) + BLOCK_SIZE_BYTES;
    }

   private:
    static constexpr size_t ALIGNMENT = alignof(T);

    static constexpr size_t BLOCK_SIZE_BYTES = 1 << 20;  // 1 MiB
    static constexpr size_t BLOCK_SIZE_ELEMS = BLOCK_SIZE_BYTES / sizeof(T);

    std::vector<T*> blocks_;
    uintptr_t currentBlockPos_ = 0;
    uintptr_t currentBlockEnd_ = 0;

    size_t count_ = 0;
};

}  // namespace neutro