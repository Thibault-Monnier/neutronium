#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <type_traits>
#include <utility>

namespace neutro {

namespace detail {
constexpr size_t DEFAULT_INLINE_BYTES = 128;

template <typename T>
consteval size_t defaultCapacity() {
    const size_t capacity = DEFAULT_INLINE_BYTES / sizeof(T);
    return capacity > 0 ? capacity : 1;  // Ensure at least 1 element
}
}  // namespace detail

template <typename T, size_t DEFAULT_CAPACITY = detail::defaultCapacity<T>()>
    requires(DEFAULT_CAPACITY > 0) && (alignof(T) <= alignof(std::max_align_t))
class SmallVector {
    static constexpr bool TRIVIAL_TYPE = std::is_trivially_copyable_v<T>;

    static constexpr size_t GROWTH_FACTOR = 2;

    T* begin_ = inlineBuffer();
    T* end_ = begin_;
    size_t capacity_ = DEFAULT_CAPACITY;

    alignas(T) std::byte inlineBuffer_[DEFAULT_CAPACITY * sizeof(T)];

    [[nodiscard]] T* inlineBuffer() { return reinterpret_cast<T*>(inlineBuffer_); }
    [[nodiscard]] const T* inlineBuffer() const {
        return reinterpret_cast<const T*>(inlineBuffer_);
    }

    [[nodiscard]] bool isInline() const {
        assert((begin_ == inlineBuffer() && capacity() == DEFAULT_CAPACITY) ||
               (begin_ != inlineBuffer() && capacity() > DEFAULT_CAPACITY));
        return begin_ == inlineBuffer();
    }

   public:
    SmallVector() = default;
    ~SmallVector() { deallocate(); }

    SmallVector(const SmallVector&) = delete;
    SmallVector& operator=(const SmallVector&) = delete;

    SmallVector(SmallVector&& other) noexcept { moveFrom(std::move(other)); }
    SmallVector& operator=(SmallVector&& other) noexcept {
        if (this != &other) {
            deallocate();
            moveFrom(std::move(other));
        }
        return *this;
    }

    [[nodiscard]] T* begin() { return begin_; }
    [[nodiscard]] const T* begin() const { return begin_; }
    [[nodiscard]] T* end() { return end_; }
    [[nodiscard]] const T* end() const { return end_; }

    [[nodiscard]] T& operator[](size_t index) {
        assert(index < size());
        return begin_[index];
    }
    [[nodiscard]] const T& operator[](size_t index) const {
        assert(index < size());
        return begin_[index];
    }

    [[nodiscard]] size_t size() const { return end() - begin(); }
    [[nodiscard]] size_t capacity() const { return capacity_; }

    [[nodiscard]] bool empty() const { return size() == 0; }

    void clear() {
        std::destroy(begin(), end());
        end_ = begin();
    }

    void push_back(const T& value) { emplace_back(value); }
    void push_back(T&& value) { emplace_back(std::move(value)); }

    template <typename... Args>
    T& emplace_back(Args&&... args) {
        if (end_ != begin() + capacity()) [[likely]] {
            assert(end_ < begin() + capacity());
            std::construct_at(end_, std::forward<Args>(args)...);

        } else {
            // `args` might be a reference to an element in the vector, which reallocation would
            // invalidate. To avoid that, we construct the object in a temporary before growing the
            // vector.
            T tmp(std::forward<Args>(args)...);
            grow();
            std::construct_at(end_, std::move(tmp));
        }

        return *(end_++);
    }

    void reserve(const size_t newCapacity) {
        if (newCapacity > capacity()) [[unlikely]] {
            grow(newCapacity);
        }
    }

   private:
    void grow() { grow(capacity() * GROWTH_FACTOR + 1); }

    void grow(const size_t newCapacity) {
        assert(newCapacity > capacity());

        const size_t sz = size();
        const size_t allocSize = newCapacity * sizeof(T);

        T* allocatedMem;
        if constexpr (TRIVIAL_TYPE) {
            if (isInline()) {
                // Copy the elements to the newly allocated memory and free the inline buffer
                allocatedMem = static_cast<T*>(std::malloc(allocSize));
                std::memcpy(allocatedMem, begin(), sz * sizeof(T));
                std::destroy(begin(), end());
            } else {
                allocatedMem = static_cast<T*>(std::realloc(begin(), allocSize));
            }
        } else {
            allocatedMem = static_cast<T*>(std::malloc(allocSize));
            std::uninitialized_move(begin(), end(), allocatedMem);
            std::destroy(begin(), end());

            if (!isInline()) std::free(begin());
        }

        begin_ = allocatedMem;
        end_ = allocatedMem + sz;
        capacity_ = newCapacity;

        assert(!isInline() && "Vector cannot be inline after growing");
    }

    /// Resets the vector to an empty state without deallocating memory.
    void reset() {
        begin_ = inlineBuffer();
        end_ = begin_;
        capacity_ = DEFAULT_CAPACITY;
    }

    /// Deallocates the memory used by the vector, if any, and resets it to an empty state.
    void deallocate() {
        [[maybe_unused]] const T* prevBegin = begin();
        clear();
        assert(begin() == prevBegin);

        if (!isInline()) [[unlikely]] {
            std::free(begin());
        }

        reset();
    }

    /// Copies the contents of `other` into this vector, leaving `other` in an empty state.
    void moveFrom(SmallVector&& other) {
        if (other.isInline()) {
            assert(capacity() == other.capacity() && "Both vectors should have `DEFAULT_CAPACITY`");
            std::uninitialized_move_n(other.inlineBuffer(), other.size(), inlineBuffer());
            end_ = inlineBuffer() + other.size();
            other.clear();
        } else {
            begin_ = other.begin_;
            end_ = other.end_;
            capacity_ = other.capacity_;
        }

        other.reset();
    }
};

}  // namespace neutro
