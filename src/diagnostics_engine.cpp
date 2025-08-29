#include "diagnostics_engine.hpp"

#include <print>

void DiagnosticsEngine::emit_error_context(const uint32_t byteOffsetStart,
                                           const uint32_t byteOffsetEnd) const {
    const std::string_view filePath = sourceManager_.get_source_file_path(fileID_);

    const auto [lineStart, columnStart] = sourceManager_.get_line_column(fileID_, byteOffsetStart);
    const auto [lineEnd, columnEnd] = sourceManager_.get_line_column(fileID_, byteOffsetEnd);

    const size_t maxLineNumberWidth = std::to_string(lineEnd).size();
    constexpr std::string_view BLUE = "\x1b[1;94m";
    constexpr std::string_view RED = "\x1b[91m";
    constexpr std::string_view RESET = "\x1b[0m";

    const std::string padding(maxLineNumberWidth, ' ');
    const std::string separator = std::format(" {}|{} ", BLUE, RESET);

    std::println("{}{}-->{} {}:{}:{}", padding, BLUE, RESET, filePath, lineStart, columnStart);
    std::println("{}{}", padding, separator);

    for (uint32_t line = lineStart; line <= lineEnd; ++line) {
        const std::string_view lineContents = sourceManager_.get_line_contents(fileID_, line);
        std::println("{}{:>{}}{}{}{}", BLUE, line, maxLineNumberWidth, RESET, separator,
                     lineContents);

        const uint32_t errorColumnStart = (line == lineStart) ? columnStart : 1;
        const uint32_t errorColumnEnd = (line == lineEnd) ? columnEnd + 1 : lineContents.size() + 1;
        std::println("{}{}{}{}{}{}", padding, separator, std::string(errorColumnStart - 1, ' '),
                     RED, std::string(errorColumnEnd - errorColumnStart, '^'), RESET);
    }

    std::println("");
}

void DiagnosticsEngine::emit_errors() const {
    std::println();
    for (const Diagnostic& diagnostic : diagnostics_) {
        if (diagnostic.level_ != Diagnostic::Level::ERROR) continue;

        constexpr std::string_view RED = "\x1b[31m";
        constexpr std::string_view BOLD = "\x1b[1m";
        constexpr std::string_view RESET = "\x1b[0m";
        std::println(std::cerr, "{}{}error:{} {}{}{}", BOLD, RED, RESET, BOLD, diagnostic.message_,
                     RESET);
        emit_error_context(diagnostic.byteOffsetStart_, diagnostic.byteOffsetEnd_);
    }
}