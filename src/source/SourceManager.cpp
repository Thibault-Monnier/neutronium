#include "SourceManager.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

std::pair<FileID, std::string_view> SourceManager::loadNewSourceFile(std::string path) {
    std::ifstream source(path, std::ios::binary);
    if (!source) {
        throw std::runtime_error("Could not open file: " + path);
    }

    const uintmax_t fileSize = std::filesystem::file_size(path);
    std::string contents(fileSize, '\0');
    source.read(contents.data(), static_cast<std::streamsize>(fileSize));
    if (!source) {
        throw std::runtime_error("Could not read file: " + path);
    }

    sourceFiles_.emplace_back(std::move(path), std::move(contents));
    return {sourceFiles_.size() - 1, sourceFiles_.back().contents()};
}

std::pair<uint32_t, uint32_t> SourceManager::getLineColumn(const FileID fileID,
                                                           const uint32_t offset) const {
    assert(fileID < sourceFiles_.size());
    assert(offset <= sourceFiles_.at(fileID).contents().size());

    const SourceFile& file = sourceFiles_[fileID];
    const auto it = std::ranges::upper_bound(file.linesStarts(), offset) - 1;
    const uint32_t line = static_cast<int>(std::distance(file.linesStarts().begin(), it));
    const uint32_t column = offset - *it;

    return {line + 1, column + 1};  // Convert to 1-based indexing
}

std::string_view SourceManager::getLineContents(const FileID fileID,
                                                const uint32_t lineNumber) const {
    const std::string& contents = sourceFiles_.at(fileID).contents();

    const auto lineStarts = sourceFiles_.at(fileID).linesStarts();
    const uint32_t lineStart = lineStarts[lineNumber - 1];
    const uint32_t nextLineStart = lineStarts[lineNumber];

    // Exclude the newline character at the end of the line
    uint32_t length = nextLineStart - lineStart;
    if (contents[nextLineStart - 1] == '\n') --length;
    return std::string_view(contents).substr(lineStart, length);
}