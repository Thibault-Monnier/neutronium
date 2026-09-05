#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <new>
#include <span>
#include <type_traits>
#include <utility>
#include <vector>

namespace neutro {

/** Arena allocator for storing elements of several trivially destructible types at once.
 *
 * Allocates elements in large blocks to reduce allocation overhead.
 * Does not call destructors on elements. Elements cannot be freed
 * individually; all memory is freed when the allocator is destroyed.
 */
class PolymorphicArenaAllocator {
   public:
    PolymorphicArenaAllocator() = default;

    PolymorphicArenaAllocator(const PolymorphicArenaAllocator&) = delete;
    PolymorphicArenaAllocator& operator=(const PolymorphicArenaAllocator&) = delete;

    PolymorphicArenaAllocator(PolymorphicArenaAllocator&&) = delete;
    PolymorphicArenaAllocator& operator=(PolymorphicArenaAllocator&&) = delete;

    ~PolymorphicArenaAllocator() { clear(); }

    /** Clears all allocated memory. */
    void clear() {
        for (void* block : blocks_) {
            ::operator delete(block, static_cast<std::align_val_t>(MAX_ALIGNMENT));
        }
        blocks_.clear();
        currentBlockPos_ = 0;
        currentBlockEnd_ = 0;
    }

    /** Insert a new element into the arena.
     *
     * @tparam T The type of the element to insert. Must be trivially destructible.
     * @param elem The element to insert.
     * @return A pointer to the newly inserted element.
     */
    template <typename T>
        requires std::is_trivially_destructible_v<T>
    T* insert(T&& elem) {
        void* mem = reinterpret_cast<void*>(allocate(sizeof(T), alignof(T)));
        return new (mem) T(std::forward<T>(elem));
    }

    /** Construct and insert a new element into the arena.
     *
     * @tparam T The type of the element to insert. Must be trivially destructible.
     * @tparam Args The types of the constructor arguments.
     * @param args The constructor arguments.
     * @return A pointer to the newly inserted element.
     */
    template <typename T, typename... Args>
        requires std::is_trivially_destructible_v<T>
    T* insert(Args&&... args) {
        return insert<T>(T(std::forward<Args>(args)...));
    }

    /** Inserts a contiguous range of trivially constructible and destructible objects into the
     * arena and returns a span to it.
     * @tparam R The type of the elements in the range.
     * @param range The contiguous range to insert.
     * @return A span to the inserted range.
     */
    template <std::ranges::contiguous_range R>
        requires std::is_trivially_copyable_v<std::ranges::range_value_t<R>> &&
                 std::is_trivially_destructible_v<std::ranges::range_value_t<R>>
    [[nodiscard]] auto insertRange(R&& range) {
        using T = std::ranges::range_value_t<R>;

        if (std::ranges::empty(range)) return std::span<T>{};

        T* data = allocateElems<T>(std::ranges::size(range));
        std::memcpy(data, std::ranges::data(range), std::ranges::size(range) * sizeof(T));
        return std::span<T>{data, std::ranges::size(range)};
    }

    /// Allocates memory for `count` elements of type `T`. Returns a pointer to the allocated
    /// memory.
    template <typename T>
        requires std::is_trivially_constructible_v<T> && std::is_trivially_destructible_v<T>
    T* allocateElems(const size_t count) {
        assert(count > 0);
        const uintptr_t mem = allocate(sizeof(T) * count, alignof(T));
        return reinterpret_cast<T*>(mem);
    }

   private:
    uintptr_t allocate(const size_t size, const size_t alignment) {
        assert((alignment & (alignment - 1)) == 0 && "Alignment must be power of two");
        assert(alignment <= MAX_ALIGNMENT && "Alignment exceeds maximum supported alignment");

        uintptr_t currentPos = currentBlockPos_;
        uintptr_t alignedPos = (currentPos + alignment - 1) & ~(alignment - 1);
        uintptr_t newPos = alignedPos + size;

        if (newPos > currentBlockEnd_) {
            allocateBlock(std::max(size, BLOCK_SIZE));

            currentPos = currentBlockPos_;
            alignedPos = (currentPos + alignment - 1) & ~(alignment - 1);
            newPos = alignedPos + size;

            assert(newPos <= currentBlockEnd_ && "New block should have enough space");
        }

        currentBlockPos_ = newPos;
        return alignedPos;
    }

    __attribute__((noinline)) void allocateBlock(const size_t size) {
        void* block = ::operator new(size, static_cast<std::align_val_t>(MAX_ALIGNMENT));
        blocks_.push_back(block);
        currentBlockPos_ = reinterpret_cast<uintptr_t>(block);
        currentBlockEnd_ = reinterpret_cast<uintptr_t>(block) + size;
    }

    static constexpr size_t MAX_ALIGNMENT = alignof(std::max_align_t);
    static constexpr size_t BLOCK_SIZE = 1 << 20;  // 1 MiB

    std::vector<void*> blocks_;
    uintptr_t currentBlockPos_ = 0;
    uintptr_t currentBlockEnd_ = 0;
};

};  // namespace neutro
