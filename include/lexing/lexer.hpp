#pragma once

#include <vector>
#include <string>

#include "lexing/token.hpp"

std::vector<Token> tokenize(const std::string &source);