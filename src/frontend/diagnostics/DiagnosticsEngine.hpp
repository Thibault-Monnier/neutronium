#pragma once

#include <algorithm>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "Diagnostic.hpp"
#include "DiagnosticsPrinter.hpp"
#include "source/FileID.hpp"
#include "source/SourceManager.hpp"

class DiagnosticsEngine {
   public:
    DiagnosticsEngine(const SourceManager& sourceManager) : diagnosticsPrinter_(sourceManager) {}

    /**
     * @brief Report an error diagnostic.
     *
     * Collects an error diagnostic with the given message and source
     * location range.
     *
     * @param message The error message.
     * @param byteOffsetStart The starting byte offset of the source location range.
     * @param byteOffsetEnd The ending byte offset of the source location range.
     * @param fileID The FileID of the source file where the error occurred.
     */
    void reportError(std::string message, const uint32_t byteOffsetStart,
                     const uint32_t byteOffsetEnd, const FileID fileID) {
        diagnostics_.emplace_back(std::move(message), byteOffsetStart, byteOffsetEnd, fileID,
                                  Diagnostic::Level::ERROR);
    }

    /**
     * @brief Emit all collected diagnostics to stderr.
     *
     * Emits all diagnostics that have been reported so far using the
     * DiagnosticsPrinter.
     */
    void emit() {
        for (const Diagnostic& diagnostic : diagnostics_) {
            diagnosticsPrinter_.emit(diagnostic);
        }
    }

    /**
     * @brief Check if any errors have been reported.
     *
     * @return @code true@endcode if at least one error diagnostic has been reported,
     * @code false@endcode otherwise.
     */
    [[nodiscard]] bool hasErrors() const {
        return std::ranges::any_of(diagnostics_, [](const Diagnostic& diagnostic) {
            return diagnostic.level_ == Diagnostic::Level::ERROR;
        });
    }

   private:
    DiagnosticsPrinter diagnosticsPrinter_;

    std::vector<Diagnostic> diagnostics_;
};