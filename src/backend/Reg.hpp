#pragma once

#include <cstdint>
#include <string>
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
    explicit Reg(const Name name, const uint32_t sizeBits) : name_(name), sizeBits_(sizeBits) {}

    [[nodiscard]] Name getName() const { return name_; }
    [[nodiscard]] uint32_t sizeBits() const { return sizeBits_; }

    [[nodiscard]] constexpr std::string toString() const { return toString(name_, sizeBits_); }

    [[nodiscard]] constexpr std::string deref() const { return "[" + toString(name_, 64) + "]"; }

   private:
    [[nodiscard]] static constexpr std::string toString(const Name name, uint32_t sizeBits) {
        sizeBits = (sizeBits + 7) / 8 * 8;

        switch (name) {
            case RAX:
                switch (sizeBits) {
                    case 8:
                        return "al";
                    case 16:
                        return "ax";
                    case 32:
                        return "eax";
                    case 64:
                        return "rax";
                    default:
                        std::unreachable();
                }
            case RBX:
                switch (sizeBits) {
                    case 8:
                        return "bl";
                    case 16:
                        return "bx";
                    case 32:
                        return "ebx";
                    case 64:
                        return "rbx";
                    default:
                        std::unreachable();
                }
            case RCX:
                switch (sizeBits) {
                    case 8:
                        return "cl";
                    case 16:
                        return "cx";
                    case 32:
                        return "ecx";
                    case 64:
                        return "rcx";
                    default:
                        std::unreachable();
                }
            case RSI:
                switch (sizeBits) {
                    case 8:
                        return "sil";
                    case 16:
                        return "si";
                    case 32:
                        return "esi";
                    case 64:
                        return "rsi";
                    default:
                        std::unreachable();
                }
            case RDI:
                switch (sizeBits) {
                    case 8:
                        return "dil";
                    case 16:
                        return "di";
                    case 32:
                        return "edi";
                    case 64:
                        return "rdi";
                    default:
                        std::unreachable();
                }
            case RSP:
                switch (sizeBits) {
                    case 8:
                        return "spl";
                    case 16:
                        return "sp";
                    case 32:
                        return "esp";
                    case 64:
                        return "rsp";
                    default:
                        std::unreachable();
                }
        }

        std::unreachable();
    }
};

}  // namespace Backend
