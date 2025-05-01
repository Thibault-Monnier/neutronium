#include "semantic-analysis/type.hpp"

#include <stdexcept>
#include <string>

std::string type_to_string(const Type type) {
    switch (type) {
        case Type::INTEGER:
            return "integer";
        case Type::BOOLEAN:
            return "boolean";
        default:
            throw std::invalid_argument("Unknown type passed to type_to_string");
    }
}