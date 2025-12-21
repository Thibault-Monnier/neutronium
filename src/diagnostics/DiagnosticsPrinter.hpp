#pragma once

#include <cstdint>
#include <format>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <utility>

#include "Diagnostic.hpp"
#include "source/FileID.hpp"
#include "source/SourceManager.hpp"

class DiagnosticsPrinter {
   public:
    explicit DiagnosticsPrinter(const SourceManager& sourceManager)
        : sourceManager_(sourceManager) {}

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

    /** @brief Records whether any diagnostics have been emitted yet. */
    bool hasEmittedAny_ = false;

    struct Params {
        static constexpr uint32_t MAX_ERROR_CONTEXT_LINES = 6;

        static constexpr std::string_view ANSI_BOLD = "\x1b[1m";
        static constexpr std::string_view ANSI_RED = "\x1b[31m";
        static constexpr std::string_view ANSI_BLUE = "\x1b[1;94m";
        static constexpr std::string_view ANSI_LIGHT_RED = "\x1b[91m";
        static constexpr std::string_view ANSI_RESET = "\x1b[0m";

        static constexpr uint8_t TAB_WIDTH = 4;
    };

    /**
     * @brief Represents a specific location within a source file.
     *
     * This class encapsulates the information necessary to identify a precise
     * position in a file, such as line and column numbers.
     */
    class SourceLocation {
        uint32_t line_;
        uint32_t column_;

       public:
        SourceLocation(const uint32_t line, const uint32_t column) : line_(line), column_(column) {}

        [[nodiscard]] uint32_t line() const { return line_; }
        [[nodiscard]] uint32_t column() const { return column_; }
    };

    /**
     * @brief Represents a span (range) within a source file.
     *
     * This class encapsulates the start and end locations of a span,
     * along with the associated file identifier.
     */
    class Span {
        SourceLocation start_;
        SourceLocation end_;
        FileID fileID_;

       public:
        Span(const SourceLocation start, const SourceLocation end, const FileID fileID)
            : start_(start), end_(end), fileID_(fileID) {}
        Span(const uint32_t startLine, const uint32_t startColumn, const uint32_t endLine,
             const uint32_t endColumn, const FileID fileID)
            : start_{startLine, startColumn}, end_{endLine, endColumn}, fileID_(fileID) {}

        [[nodiscard]] SourceLocation start() const { return start_; }
        [[nodiscard]] SourceLocation end() const { return end_; }

        [[nodiscard]] uint32_t startLine() const { return start_.line(); }
        [[nodiscard]] uint32_t startColumn() const { return start_.column(); }
        [[nodiscard]] uint32_t endLine() const { return end_.line(); }
        [[nodiscard]] uint32_t endColumn() const { return end_.column(); }

        [[nodiscard]] FileID fileID() const { return fileID_; }
    };

    /**
     * @brief Represents the layout information for a diagnostic.
     *
     * This class encapsulates all the information used for emitting a diagnostic.
     */
    class Layout {
        const Diagnostic* diag_;
        Span span_;
        uint32_t gutterWidth_;

       public:
        Layout(const Diagnostic& diag, const Span span, const uint32_t gutterWidth)
            : diag_(&diag), span_(span), gutterWidth_(gutterWidth) {}

        [[nodiscard]] const Diagnostic& diag() const { return *diag_; }
        [[nodiscard]] const Span& span() const { return span_; }
        [[nodiscard]] uint32_t gutterWidth() const { return gutterWidth_; }
    };

    /**
     * @brief The current diagnostic layout.
     *
     * This should be set before starting to emit a diagnostic.
     */
    std::optional<Layout> diagLayout_;

    /**
     * @brief Get the current diagnostic layout.
     *
     * If no layout has been computed yet, this function will throw an exception.
     *
     * @return The current diagnostic layout.
     */
    [[nodiscard]] const Layout& diagLayout() const { return diagLayout_.value(); }

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

    [[nodiscard]] std::string blankGutter() const {
        return std::string(diagLayout().gutterWidth(), ' ');
    }

    /** @brief Get the number of bytes in a UTF-8 character given its first byte.
     *
     * @param c The first byte of the UTF-8 character.
     * @return The number of bytes in the UTF-8 character.
     */
    static uint32_t utf8ByteLength(const unsigned char c) {
        if ((c & 0x80) == 0) return 1;     // ASCII
        if ((c & 0xE0) == 0xC0) return 2;  // 2-byte UTF-8
        if ((c & 0xF0) == 0xE0) return 3;  // 3-byte UTF-8
        if ((c & 0xF8) == 0xF0) return 4;  // 4-byte UTF-8

        std::unreachable();
    }

    // -------------------------------------
    // --- Diagnostic emission functions ---
    // -------------------------------------

    /**
     * @brief Compute the layout for a diagnostic.
     *
     * This function computes the layout for the provided diagnostic and stores it in diagLayout_.
     * It should be called before emitting the diagnostic.
     *
     * @param diagnostic The diagnostic to compute the layout for.
     */
    void computeDiagLayout(const Diagnostic& diagnostic);

    /** @brief Normalize line contents by using constant width characters.
     *
     * @param lineContents The line contents which are normalized in place.
     */
    static void normalizeLineContents(std::string& lineContents);

    /** @brief Compute the column span to underline for a line of a diagnostic.
     *
     * @param line The line number to compute the underline span for.
     * @param lineContents The contents of the line.
     * @return A pair (startColumn, endColumn) representing the column range to underline
     */
    std::pair<uint32_t, uint32_t> computeUnderlineColumnRange(uint32_t line,
                                                              std::string_view lineContents) const;

    void emitError() const;
    void emitErrorMessage() const;
    void emitErrorLocation() const;
    void emitErrorContext() const;
};
