#include "source/source_manager.hpp"

#include <algorithm>
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

    std::vector<int> lineStarts;
    lineStarts.push_back(0);
    for (int i = 0; i < contents.size() - 1; ++i) {
        if (contents[i] == '\n') {
            lineStarts.push_back(i + 1);
        }
    }
    sourceFiles_.emplace_back(std::move(path), std::move(contents), std::move(lineStarts));
    return {static_cast<int>(sourceFiles_.size() - 1), sourceFiles_.back().contents()};
}

std::pair<int, int> SourceManager::get_line_column(const int fileID, const uint32_t offset) const {
    const SourceFile& file = sourceFiles_[fileID];
    const auto it = std::ranges::upper_bound(file.lines_starts(), offset) - 1;
    const int line = static_cast<int>(std::distance(file.lines_starts().begin(), it));
    const int column = offset - *it;
    return {line + 1, column + 1};  // Convert to 1-based indexing
}