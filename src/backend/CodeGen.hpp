#pragma once
#include "ir/core/IR.hpp"
#include "lib/FastStringStream.hpp"

namespace Backend {

/// Consumes IR and produces X86-64 assembly.
class CodeGen {
    const IR::Module& ir_;

    neutro::FastStringStream output_;

   public:
    explicit CodeGen(const IR::Module& ir) : ir_(ir) {}

    [[nodiscard]] neutro::FastStringStream generate();
};

}  // namespace Backend
