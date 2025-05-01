#pragma once

#include <string>
#include <unordered_map>
#include <unordered_set>

struct Scope {
    std::unordered_map<std::string, int> variablesStackOffset_;
    std::unordered_set<std::string> symbols_;
    int frameSize_ = 0;
};