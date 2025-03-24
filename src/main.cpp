#include <fstream>
#include <iostream>
#include <string>

void print_error(const std::string &message) {
    std::cerr << "\033[31m" << "Error: " << "\033[0m" << message;
}

void print_hint(const std::string &message) {
    std::cout << "\033[33m" << "Hint: " << "\033[0m" << message;
}

int main(const int argc, char *argv[]) {
    if (argc != 2) {
        const std::string errorMessage =
            "Expected 1 argument, got " + std::to_string(argc - 1) + '\n';
        print_error(errorMessage);

        const std::string hintMessage = "Usage: <filename>\n";
        print_hint(hintMessage);

        return 1;
    }

    std::ifstream fileStream("../" + std::string(argv[1]));
    if (!fileStream.is_open()) {
        const std::string errorMessage = "Could not open file " + std::string(argv[1]) + '\n';
        print_error(errorMessage);

        return 1;
    }

    std::string fileContents;
    std::string line;
    while (std::getline(fileStream, line)) {
        fileContents += line + '\n';
    }

    std::cout << fileContents;

    return 0;
}
