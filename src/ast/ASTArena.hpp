#pragma once

#include <memory>
#include <vector>

class ASTArena {
   public:
    ASTArena() = default;
    ~ASTArena() = default;

    template <typename T, typename... Args>
        requires std::derived_from<T, AST::Node>
    T* allocate(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        nodes_.push_back(std::move(node));
        return ptr;
    }

    template <typename T, typename... Args>
        requires std::derived_from<T, AST::Node>
    const T* allocate(const Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        const T* ptr = node.get();
        nodes_.push_back(std::move(node));
        return ptr;
    }

   private:
    std::vector<std::unique_ptr<AST::Node>> nodes_;
};
