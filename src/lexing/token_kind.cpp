#include "lexing/token_kind.hpp"

#include <string>

#include "magic_enum.hpp"

std::string token_kind_to_string(const TokenKind kind) {
    const auto name = magic_enum::enum_name(kind);
    return std::string{name};
}
