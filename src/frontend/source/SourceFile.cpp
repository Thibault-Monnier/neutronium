#include <immintrin.h>

#include <cassert>
#include <cstddef>
#include <cstdint>

#include "SourceManager.hpp"

void SourceManager::SourceFile::nbLinesEstimate() const {
    size_t estimate = contents_.size() / 15;  // Generous estimate: 15 chars / line
    if (estimate < 32) estimate = 32;         // Minimum capacity
    lineStarts_.reserve(estimate);
}

void SourceManager::SourceFile::scanFileLineStarts() const {
    assert(lineStarts_.empty() && "Re-scanning file line starts");

    const char* data = contents_.data();
    const size_t size = contents_.size();

    nbLinesEstimate();
    lineStarts_.push_back(0);

    size_t i = 0;

#ifdef __AVX2__
    // Fast path if available
    // Process 32 bytes at a time using AVX2 intrinsics

    const __m256i newline = _mm256_set1_epi8('\n');

    for (; i + 32 <= size; i += 32) {
        const __m256i chunk = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(data + i));
        const __m256i cmp = _mm256_cmpeq_epi8(chunk, newline);

        uint32_t mask = _mm256_movemask_epi8(cmp);

        while (mask != 0) {
            const uint32_t bit = __builtin_ctz(mask);
            lineStarts_.push_back(i + bit + 1);
            mask &= mask - 1;
        }
    }

#endif
    // Process remaining bytes
    for (; i < size; ++i) {
        if (data[i] == '\n') {
            lineStarts_.push_back(i + 1);
        }
    }

    // Add a sentinel value for easier calculations later
    lineStarts_.push_back(static_cast<int>(size) + 1);

    lineStarts_.shrink_to_fit();
}