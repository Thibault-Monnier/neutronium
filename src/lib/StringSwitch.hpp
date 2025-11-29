#pragma once

#include <optional>

#include "FastStringView.hpp"

namespace neutro {

/// A class that allows switching over string values at runtime, similar to a switch-case statement.
/// It allows for better compiler optimizations than a series of if-else statements.
template <typename T>
class StringSwitch {
    const FastStringView string_;

    std::optional<T> result_;

   public:
    explicit StringSwitch(const FastStringView string) : string_(string) {}

    StringSwitch(const StringSwitch&) = delete;
    void operator=(const StringSwitch&) = delete;
    void operator=(StringSwitch&& other) = delete;

    /** @brief Adds a case to the string switch.
     *
     * This method checks whether the input string matches the provided case string and updates
     * the result if it does.
     *
     * @param caseString The string to match against.
     * @param value The value taken if the case matches.
     * @return A reference to the class instance allowing for method chaining.
     **/
    StringSwitch& newCase(const FastStringView caseString, const T value) {
        if (!result_ && string_ == caseString) {
            result_ = std::move(value);
        }
        return *this;
    }

    /// Returns the result of the string switch, or the provided default value if no case matched.
    T defaultCase(const T value) { return result_.value_or(std::move(value)); }
};

}  // namespace neutro