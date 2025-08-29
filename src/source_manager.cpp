#include "source_manager.hpp"

#include <algorithm>
#include <cassert>
#include <fstream>
#include <sstream>
#include <stdexcept>

std::pair<int, std::string_view> SourceManager::load_new_source_file(std::string path) {
    const std::ifstream source(path);
    if (!source) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << source.rdbuf();
    std::string contents = buffer.str();

    // Precompute line start indices
    // The first line always starts at index 0
    std::vector<uint32_t> lineStarts;
    lineStarts.push_back(0);
    for (size_t i = 0; i < contents.size(); ++i) {
        if (contents[i] == '\n') {
            lineStarts.push_back(i + 1);
        }
    }
    // Add a sentinel value for easier calculations later
    lineStarts.push_back(static_cast<int>(contents.size()) + 1);

    sourceFiles_.emplace_back(std::move(path), std::move(contents), std::move(lineStarts));
    return {static_cast<int>(sourceFiles_.size() - 1), sourceFiles_.back().contents()};
}

std::pair<uint32_t, uint32_t> SourceManager::get_line_column(const int fileID,
                                                             const uint32_t offset) const {
    assert(fileID >= 0 && fileID < static_cast<int>(sourceFiles_.size()));
    assert(offset <= sourceFiles_[fileID].contents().size());

    const SourceFile& file = sourceFiles_[fileID];
    const auto it = std::ranges::upper_bound(file.lines_starts(), offset) - 1;
    const uint32_t line = static_cast<int>(std::distance(file.lines_starts().begin(), it));
    const uint32_t column = offset - *it;

    return {line + 1, column + 1};  // Convert to 1-based indexing
}

std::string_view SourceManager::get_line_contents(const int fileID,
                                                  const uint32_t lineNumber) const {
    const std::string& contents = sourceFiles_[fileID].contents();

    const auto lineStarts = sourceFiles_[fileID].lines_starts();
    const uint32_t lineStart = lineStarts[lineNumber - 1];
    const uint32_t nextLineStart = lineStarts[lineNumber];

    // Exclude the newline character at the end of the line
    uint32_t length = nextLineStart - lineStart;
    if (contents[nextLineStart - 1] == '\n') --length;
    return std::string_view(contents).substr(lineStart, length);
}