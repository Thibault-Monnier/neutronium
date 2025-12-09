#include "Log.hpp"

#include <iostream>
#include <string>

void printError(const std::string& message) {
    std::cerr << "\033[1;31m" << "error: " << "\033[0m" << message << '\n';
}

void printWarning(const std::string& message) {
    std::cerr << "\033[1;33m" << "warning: " << "\033[0m" << message << '\n';
}