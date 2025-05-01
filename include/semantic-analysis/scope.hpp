#pragma once

#include <unordered_map>
#include <string>

struct Scope {
    std::unordered_map<std::string, int> variablesStackOffset_;
    int frameSize_;
};