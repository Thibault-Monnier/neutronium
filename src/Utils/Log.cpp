#include "Utils/Log.hpp"

#include <iostream>
#include <string>

void print_error(const std::string &message) {
    std::cerr << "\n\033[1;31m" << "error: " << "\033[0m" << message << '\n';
}
