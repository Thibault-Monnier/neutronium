#pragma once

#include <cstddef>
#include <format>
#include <print>

/**
 * @brief Macro for counting iterations and logging progress.
 *
 * This macro creates a static instance of `IterationCounterImpl` and increments the count each time
 * it is called. It logs the current iteration count at specified intervals, which can be customized
 * through the constructor of `IterationCounterImpl`.
 *
 * @param ... Arguments forwarded to the constructor of `IterationCounterImpl`, allowing for
 * customization of the log format and interval.
 *
 * @return The current iteration count after incrementing.
 */
#define iterationCounter(...)                             \
    ([]() {                                               \
        static IterationCounterImpl counter{__VA_ARGS__}; \
        return counter.increment();                       \
    }())

class IterationCounterImpl {
    size_t count_ = 0;

    std::format_string<size_t> fmt_;
    size_t logInterval_;

   public:
    explicit IterationCounterImpl(const std::format_string<size_t> fmt = "Iteration {}",
                                  const size_t logInterval = 1)
        : fmt_(fmt), logInterval_(logInterval) {}

    IterationCounterImpl(const IterationCounterImpl&) = delete;
    IterationCounterImpl& operator=(const IterationCounterImpl&) = delete;

    size_t increment() {
        ++count_;
        if (count_ % logInterval_ == 0) {
            std::print(fmt_, +count_);
            std::println();
        }
        return count_;
    }
};
