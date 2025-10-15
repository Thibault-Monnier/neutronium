#include "lexing/token_kind.hpp"

#include <magic_enum/magic_enum.hpp>
#include <string>

std::string token_kind_to_string(const TokenKind kind) {
    const auto name = magic_enum::enum_name(kind);
    return std::string{name};
}
