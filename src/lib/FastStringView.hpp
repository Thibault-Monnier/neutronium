#pragma once

#include <string>
#include <string_view>

namespace neutro {

/// A lightweight string view class optimized for fast comparisons, at the cost of safety. It
/// maximizes the use of constexpr and is designed to be inline-friendly to the compiler.
class FastStringView {
    const char* const data_;
    const size_t length_;

   public:
    /*implicit*/ constexpr FastStringView(const char* const str)
        : data_(str), length_(std::char_traits<char>::length(str)) {}

    /*implicit*/ constexpr FastStringView(const char* const data, const size_t length)
        : data_(data), length_(length) {}

    /*implicit*/ FastStringView(const std::string& str)
        : data_(str.data()), length_(str.length()) {}

    /*implicit*/ constexpr FastStringView(const std::string_view str)
        : data_(str.data()), length_(str.size()) {}

    [[nodiscard]] constexpr const char* data() const { return data_; }

    [[nodiscard]] constexpr bool empty() const { return size() == 0; }
    [[nodiscard]] constexpr size_t size() const { return length_; }
};

inline bool operator==(const FastStringView lhs, const FastStringView rhs) {
    const size_t lhsSize = lhs.size();
    if (lhsSize != rhs.size()) return false;
    if (lhsSize == 0) [[unlikely]]
        return true;

    return __builtin_memcmp(lhs.data(), rhs.data(), lhsSize) == 0;
}

inline bool operator!=(const FastStringView lhs, const FastStringView rhs) { return !(lhs == rhs); }

};  // namespace neutro