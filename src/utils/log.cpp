#include <iostream>
#include <string>

void print_error(const std::string &message) {
    std::cerr << "\033[31m" << "Error: " << "\033[0m" << message << '\n';
}

void print_hint(const std::string &message) {
    std::cout << "\033[33m" << "Hint: " << "\033[0m" << message << '\n';
}