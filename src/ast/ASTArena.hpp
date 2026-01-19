#pragma once

#include <algorithm>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>
#include <vector>

#include "AST.hpp"

class ASTArena {
   public:
    ASTArena() = default;

    ASTArena(const ASTArena&) = delete;
    ASTArena& operator=(const ASTArena&) = delete;

    ASTArena(ASTArena&&) = delete;
    ASTArena& operator=(ASTArena&&) = delete;

    ~ASTArena() {
        for (void* block : blocks_) {
            ::operator delete(block, static_cast<std::align_val_t>(MAX_ALIGNMENT));
        }
    }

    template <typename T, typename... Args>
        requires std::derived_from<T, AST::Node> && std::is_trivially_destructible_v<T>
    T* insert(Args&&... args) {
        void* mem = reinterpret_cast<void*>(allocate(sizeof(T), alignof(T)));
        return new (mem) T(std::forward<Args>(args)...);
    }

    template <typename T>
        requires std::is_trivially_constructible_v<T> && std::is_trivially_destructible_v<T>
    T* insertArray(const size_t count) {
        uintptr_t mem = allocate(sizeof(T) * count, alignof(T));
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

    void allocateBlock(const size_t size) {
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
