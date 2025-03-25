#pragma once

#include <optional>
#include <string>

#include "token_type.hpp"

struct Token {
    TokenType type;
    std::optional<std::string> value;
};