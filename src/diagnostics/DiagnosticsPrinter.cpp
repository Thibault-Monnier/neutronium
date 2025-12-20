#include "DiagnosticsPrinter.hpp"

#include <cassert>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <string_view>

#include "Diagnostic.hpp"

void DiagnosticsPrinter::emit(const Diagnostic& diagnostic) {
    if (!hasEmittedAny_) {
        hasEmittedAny_ = true;
        println();
    }

    computeDiagLayout(diagnostic);

    switch (diagnostic.level_) {
        case Diagnostic::Level::ERROR:
            emitError();
            break;
    }

    diagLayout_.reset();

    println();
}

void DiagnosticsPrinter::computeDiagLayout(const Diagnostic& diagnostic) {
    const auto [lineStart, columnStart] =
        sourceManager_.getLineColumn(diagnostic.fileID_, diagnostic.byteOffsetStart_);
    const auto [lineEnd, columnEnd] =
        sourceManager_.getLineColumn(diagnostic.fileID_, diagnostic.byteOffsetEnd_);
    const Span span(lineStart, columnStart, lineEnd, columnEnd, diagnostic.fileID_);

    const uint32_t gutterWidth = std::to_string(lineEnd).size();

    diagLayout_ = Layout(diagnostic, span, gutterWidth);
}

void DiagnosticsPrinter::emitError() const {
    assert(diagLayout_.has_value() && "diagLayout_ should be initialized");
    assert(diagLayout().diag().level_ == Diagnostic::Level::ERROR);

    emitErrorMessage();
    emitErrorLocation();
    emitErrorContext();
}

void DiagnosticsPrinter::emitErrorMessage() const {
    println("{}{}error:{} {}{}{}", Params::ANSI_BOLD, Params::ANSI_RED, Params::ANSI_RESET,
            Params::ANSI_BOLD, diagLayout().diag().message_, Params::ANSI_RESET);
}

void DiagnosticsPrinter::emitErrorLocation() const {
    const std::string_view filePath =
        sourceManager_.getSourceFilePath(diagLayout().span().fileID());

    println("{}{}-->{} {}:{}:{}", blankGutter(), Params::ANSI_BLUE, Params::ANSI_RESET, filePath,
            diagLayout().span().startLine(), diagLayout().span().startColumn());
}

void DiagnosticsPrinter::emitErrorContext() const {
    const Span& span = diagLayout().span();

    const uint32_t startLine = span.startLine();
    const uint32_t endLine = span.endLine();

    const uint32_t nbLines = endLine - startLine + 1;

    const bool skipMiddleLines = nbLines > Params::MAX_ERROR_CONTEXT_LINES;
    // MAX_ERROR_CONTEXT_LINES - 1 to account for the "lines omitted" message
    const uint32_t nbOmittedLines = nbLines - (Params::MAX_ERROR_CONTEXT_LINES - 1);
    const uint32_t skipLinesStart = startLine + Params::MAX_ERROR_CONTEXT_LINES / 2;
    const uint32_t skipLinesEnd = skipLinesStart + nbOmittedLines;
    assert(nbOmittedLines == skipLinesEnd - skipLinesStart);

    const std::string separator = std::format(" {}|{} ", Params::ANSI_BLUE, Params::ANSI_RESET);
    println("{}{}", blankGutter(), separator);

    for (uint32_t line = startLine; line <= endLine; ++line) {
        if (skipMiddleLines && line == skipLinesStart) {
            println("{}{}", blankGutter(), separator);

            std::string linesOmittedPadding(diagLayout().gutterWidth() - 1, ' ');
            println("{}{}... {} line{} omitted ...", Params::ANSI_BLUE, linesOmittedPadding,
                    nbOmittedLines, nbOmittedLines > 1 ? "s" : "");

            println("{}{}", blankGutter(), separator);
            line = skipLinesEnd;
        }

        const std::string_view lineContents = sourceManager_.getLineContents(span.fileID(), line);
        println("{}{:>{}}{}{}{}", Params::ANSI_BLUE, line, diagLayout().gutterWidth(),
                Params::ANSI_RESET, separator, lineContents);

        const uint32_t errorColumnStart = (line == startLine) ? span.startColumn() : 1;
        const uint32_t errorColumnEnd =
            (line == endLine) ? span.endColumn() + 1 : lineContents.size() + 1;
        println("{}{}{}{}{}{}", blankGutter(), separator, std::string(errorColumnStart - 1, ' '),
                Params::ANSI_LIGHT_RED, std::string(errorColumnEnd - errorColumnStart, '^'),
                Params::ANSI_RESET);
    }
}