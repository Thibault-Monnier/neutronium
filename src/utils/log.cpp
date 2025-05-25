#include "utils/log.hpp"

#include <iostream>
#include <string>

void print_error(const std::string &message) {
    std::cerr << "\n\033[31m" << "Error: " << "\033[0m" << message << '\n';
}
