#pragma once

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

#include "Type.hpp"

class TypeArena {
   public:
    TypeArena() = default;
    ~TypeArena() = default;

    void push(std::unique_ptr<Type> type) { types_.push_back(std::move(type)); }

    [[nodiscard]] Type& at(const size_t index) const { return *types_.at(index); }

    [[nodiscard]] size_t size() const { return types_.size(); }

   private:
    std::vector<std::unique_ptr<Type>> types_;
};
