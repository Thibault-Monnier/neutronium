#include "DiagnosticsPrinter.hpp"

#include <cassert>
#include <cstdint>
#include <format>
#include <iostream>
#include <print>
#include <string>
#include <string_view>

#include "Diagnostic.hpp"

void DiagnosticsPrinter::emit(const Diagnostic& diagnostic) {
    if (!hasEmittedAny_) {
        hasEmittedAny_ = true;
        std::println();
    }

    switch (diagnostic.level_) {
        case Diagnostic::Level::ERROR:
            emitError(diagnostic);
            break;
    }

    std::println();
}

void DiagnosticsPrinter::emitError(const Diagnostic& diagnostic) const {
    assert(diagnostic.level_ == Diagnostic::Level::ERROR);

    std::println(std::cerr, "{}{}error:{} {}{}{}", Params::ANSI_BOLD, Params::ANSI_RED,
                 Params::ANSI_RESET, Params::ANSI_BOLD, diagnostic.message_, Params::ANSI_RESET);
    emitErrorContext(diagnostic.byteOffsetStart_, diagnostic.byteOffsetEnd_);
}

void DiagnosticsPrinter::emitErrorContext(const uint32_t byteOffsetStart,
                                          const uint32_t byteOffsetEnd) const {
    const std::string_view filePath = sourceManager_.getSourceFilePath(fileID_);

    const auto [lineStart, columnStart] = sourceManager_.getLineColumn(fileID_, byteOffsetStart);
    const auto [lineEnd, columnEnd] = sourceManager_.getLineColumn(fileID_, byteOffsetEnd);

    const uint32_t maxLineNumberWidth = std::to_string(lineEnd).size();

    const std::string padding(maxLineNumberWidth, ' ');
    const std::string separator = std::format(" {}|{} ", Params::ANSI_BLUE, Params::ANSI_RESET);

    std::println("{}{}-->{} {}:{}:{}", padding, Params::ANSI_BLUE, Params::ANSI_RESET, filePath,
                 lineStart, columnStart);
    std::println("{}{}", padding, separator);

    const uint32_t nbLines = lineEnd - lineStart + 1;

    const bool skipMiddleLines = nbLines > Params::MAX_ERROR_CONTEXT_LINES;
    // MAX_ERROR_CONTEXT_LINES - 1 to account for the "lines omitted" message
    const uint32_t nbOmittedLines = nbLines - (Params::MAX_ERROR_CONTEXT_LINES - 1);
    const uint32_t skipLinesStart = lineStart + Params::MAX_ERROR_CONTEXT_LINES / 2;
    const uint32_t skipLinesEnd = skipLinesStart + nbOmittedLines;
    assert(nbOmittedLines == skipLinesEnd - skipLinesStart);

    for (uint32_t line = lineStart; line <= lineEnd; ++line) {
        if (skipMiddleLines && line == skipLinesStart) {
            std::println("{}{}", padding, separator);

            std::string linesOmittedPadding(maxLineNumberWidth - 1, ' ');
            std::println("{}{}... {} line{} omitted ...", Params::ANSI_BLUE, linesOmittedPadding,
                         nbOmittedLines, nbOmittedLines > 1 ? "s" : "");

            std::println("{}{}", padding, separator);
            line = skipLinesEnd;
        }

        const std::string_view lineContents = sourceManager_.getLineContents(fileID_, line);
        std::println("{}{:>{}}{}{}{}", Params::ANSI_BLUE, line, maxLineNumberWidth,
                     Params::ANSI_RESET, separator, lineContents);

        const uint32_t errorColumnStart = (line == lineStart) ? columnStart : 1;
        const uint32_t errorColumnEnd = (line == lineEnd) ? columnEnd + 1 : lineContents.size() + 1;
        std::println("{}{}{}{}{}{}", padding, separator, std::string(errorColumnStart - 1, ' '),
                     Params::ANSI_LIGHT_RED, std::string(errorColumnEnd - errorColumnStart, '^'),
                     Params::ANSI_RESET);
    }
}