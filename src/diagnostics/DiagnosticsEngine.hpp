#pragma once
#include <algorithm>
#include <iostream>

#include "source/SourceManager.hpp"

struct Diagnostic {
    enum class Level : uint8_t { ERROR };

    const std::string message_;
    const int byteOffsetStart_;
    const int byteOffsetEnd_;
    Level level_;
};

class DiagnosticsEngine {
   public:
    DiagnosticsEngine(const SourceManager& sourceManager, const int fileID)
        : sourceManager_(sourceManager), fileID_(fileID) {}

    void reportError(std::string message, const uint32_t byteOffsetStart,
                     const uint32_t byteOffsetEnd) {
        diagnostics_.emplace_back(std::move(message), byteOffsetStart, byteOffsetEnd,
                                  Diagnostic::Level::ERROR);
    }

    void emitErrors() const;

    [[nodiscard]] bool hasErrors() const {
        return std::ranges::any_of(diagnostics_, [](const Diagnostic& diagnostic) {
            return diagnostic.level_ == Diagnostic::Level::ERROR;
        });
    }

   private:
    const SourceManager& sourceManager_;
    const int fileID_;

    std::vector<Diagnostic> diagnostics_;

    void emitErrorContext(uint32_t byteOffsetStart, uint32_t byteOffsetEnd) const;
};