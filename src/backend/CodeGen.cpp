#include "CodeGen.hpp"

namespace Backend {

neutro::FastStringStream CodeGen::generate() {
    for (const std::unique_ptr<IR::Function>& func : ir_.getFunctions()) {
    }

    output_ << "Hello, World!\n";

    return std::move(output_);
}

}  // namespace Backend
