#pragma once

#include <concepts>
#include <cstddef>
#include <string>
#include <string_view>
#include <type_traits>

namespace neutro {

/**
 * @brief A fast string stream optimized for appending strings and integral types.
 */
class FastStringStream {
    std::string buffer_;

   public:
    FastStringStream() = default;
    ~FastStringStream() = default;
    FastStringStream(const FastStringStream&) = delete;
    FastStringStream& operator=(const FastStringStream&) = delete;
    FastStringStream(FastStringStream&&) = default;
    FastStringStream& operator=(FastStringStream&&) = default;

    /// @brief Appends a string or string-like object to the stream.
    template <typename T>
        requires std::is_convertible_v<T, std::string_view>
    FastStringStream& operator<<(const T& str) {
        return append(str);
    }

    /** @brief Appends an integral value to the stream.
     *
     * If the type is a character, it is appended directly. Otherwise, it is converted to a string
     * representation first.
     **/
    template <std::integral T>
    FastStringStream& operator<<(T n) {
        if constexpr (std::is_same_v<std::remove_cvref_t<T>, char> ||
                      std::is_same_v<std::remove_cvref_t<T>, signed char> ||
                      std::is_same_v<std::remove_cvref_t<T>, unsigned char>) {
            return append(static_cast<char>(n));
        } else if constexpr (std::is_signed_v<T>) {
            return append(std::to_string(static_cast<long long>(n)));
        } else {
            return append(std::to_string(static_cast<unsigned long long>(n)));
        }
    }

    /// @brief Returns a constant reference to the internal string buffer.
    [[nodiscard]] const std::string& str() const { return buffer_; }

   private:
    template <typename T>
        requires std::is_convertible_v<T, std::string_view>
    FastStringStream& append(const T& value) {
        buffer_.append(value);
        return *this;
    }

    FastStringStream& append(const char c) {
        buffer_.push_back(c);
        return *this;
    }
};

}  // namespace neutro