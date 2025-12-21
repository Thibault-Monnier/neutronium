#include "SourceManager.hpp"

#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#include <algorithm>
#include <cassert>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <format>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

#include "FileID.hpp"

std::pair<FileID, std::string_view> SourceManager::loadNewSourceFile(std::string path) {
    const int fd = open(path.c_str(), O_RDONLY);
    if (fd == -1) {
        throw std::runtime_error(
            std::format("Could not open file `{}`: {}", path, std::strerror(errno)));
    }

    const size_t fileSize = std::filesystem::file_size(path);

    const void* ptr = mmap(nullptr, fileSize, PROT_READ, MAP_PRIVATE, fd, 0);
    if (ptr == MAP_FAILED) {
        throw std::runtime_error(
            std::format("Could not mmap file `{}`: {}", path, std::strerror(errno)));
    }
    close(fd);

    sourceFiles_.emplace_back(std::move(path),
                              std::string_view(static_cast<const char*>(ptr), fileSize));
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

    return {line, column};  // 0-based
}

std::string_view SourceManager::getLineContents(const FileID fileID,
                                                const uint32_t lineNumber) const {
    assert(fileID < sourceFiles_.size());
    assert(lineNumber < sourceFiles_.at(fileID).linesStarts().size());

    const std::string_view contents = sourceFiles_[fileID].contents();

    const auto lineStarts = sourceFiles_[fileID].linesStarts();
    const uint32_t lineStart = lineStarts[lineNumber];
    const uint32_t nextLineStart = lineStarts[lineNumber + 1];

    // Exclude the newline character at the end of the line
    uint32_t length = nextLineStart - lineStart;
    if (contents[nextLineStart - 1] == '\n') --length;
    return contents.substr(lineStart, length);
}