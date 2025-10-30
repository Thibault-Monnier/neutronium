#include "TokenKind.hpp"

#include <magic_enum/magic_enum.hpp>
#include <string>

std::string tokenKindToString(const TokenKind kind) {
    const auto name = magic_enum::enum_name(kind);
    return std::string{name};
}
