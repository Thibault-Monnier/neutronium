#pragma once

#include <array>
#include <bit>
#include <cassert>
#include <cstdint>
#include <string>
#include <string_view>
#include <utility>

namespace Backend {

class Reg {
   public:
    enum class Name : uint8_t {
        RAX,
        RBX,
        RCX,
        RSI,
        RDI,
        RSP,
    };
    using enum Name;

   private:
    Name name_;
    uint32_t sizeBits_;

   public:
    explicit constexpr Reg(const Name name, const uint32_t sizeBits)
        : name_(name), sizeBits_(sizeBits) {}

    [[nodiscard]] constexpr Name getName() const { return name_; }
    [[nodiscard]] constexpr uint32_t sizeBits() const { return sizeBits_; }

    [[nodiscard]] constexpr std::string_view toString() const { return toString(name_, sizeBits_); }

    [[nodiscard]] constexpr std::string deref() const {
        return "[" + std::string(toString(name_, 64)) + "]";
    }

   private:
    [[nodiscard]] static constexpr std::string_view toString(const Name name, uint32_t sizeBits) {
        sizeBits = (sizeBits + 7) / 8 * 8;

        static constexpr std::array TABLE = {
            // 8-bit, 16-bit, 32-bit, 64-bit
            std::array<std::string_view, 4>{"al", "ax", "eax", "rax"},   // RAX
            std::array<std::string_view, 4>{"bl", "bx", "ebx", "rbx"},   // RBX
            std::array<std::string_view, 4>{"cl", "cx", "ecx", "rcx"},   // RCX
            std::array<std::string_view, 4>{"sil", "si", "esi", "rsi"},  // RSI
            std::array<std::string_view, 4>{"dil", "di", "edi", "rdi"},  // RDI
            std::array<std::string_view, 4>{"spl", "sp", "esp", "rsp"},  // RSP
        };

        assert((sizeBits == 8 || sizeBits == 16 || sizeBits == 32 || sizeBits == 64) &&
               "Invalid register size");
        assert(static_cast<size_t>(name) < TABLE.size() && "Invalid register name");

        const size_t sizeIdx = std::countr_zero(sizeBits) - 3;
        assert(sizeIdx < 4 && "Invalid size index");
        return TABLE[static_cast<size_t>(name)][sizeIdx];
    }
};

}  // namespace Backend
