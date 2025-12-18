#pragma once

#include <cstdint>
#include <format>
#include <iostream>
#include <print>
#include <string_view>

#include "Diagnostic.hpp"
#include "source/FileID.hpp"
#include "source/SourceManager.hpp"

class DiagnosticsPrinter {
   public:
    explicit DiagnosticsPrinter(const SourceManager& sourceManager, const FileID fileID)
        : sourceManager_(sourceManager), fileID_(fileID) {}

    /**
     * @brief Emit a single diagnostic to stderr.
     *
     * Prints the diagnostic message and its associated source context
     * (annotated lines and carets). Supports all diagnostic levels.
     *
     * A blank line is printed before the
     * first diagnostic to separate diagnostics from normal output, and
     * a blank line is printed after each diagnostic.
     *
     * Output is written exclusively to stderr.
     *
     * @param diagnostic The diagnostic to emit.
     */
    void emit(const Diagnostic& diagnostic);

   private:
    const SourceManager& sourceManager_;
    const FileID fileID_;

    bool hasEmittedAny_ = false;

    struct Params {
        static constexpr uint32_t MAX_ERROR_CONTEXT_LINES = 6;

        static constexpr std::string_view ANSI_BOLD = "\x1b[1m";
        static constexpr std::string_view ANSI_RED = "\x1b[31m";
        static constexpr std::string_view ANSI_BLUE = "\x1b[1;94m";
        static constexpr std::string_view ANSI_LIGHT_RED = "\x1b[91m";
        static constexpr std::string_view ANSI_RESET = "\x1b[0m";
    };

    // ----------------------------
    // ----- Helper functions -----
    // ----------------------------

    /**
     * @brief Print a formatted line to stderr.
     *
     * Wrapper around std::println that always writes to std::cerr.
     *
     * @tparam Args The types of the format arguments.
     * @param fmt The format string.
     * @param args The format arguments.
     */
    template <class... Args>
    static void println(std::format_string<Args...> fmt, Args&&... args) {
        std::println(std::cerr, fmt, std::forward<Args>(args)...);
    }

    /**
     * @brief Print a blank line to stderr.
     */
    static void println() { std::println(std::cerr); }

    // -------------------------------------
    // --- Diagnostic emission functions ---
    // -------------------------------------

    void emitError(const Diagnostic& diagnostic) const;
    void emitErrorContext(uint32_t byteOffsetStart, uint32_t byteOffsetEnd) const;
};
