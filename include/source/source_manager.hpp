#pragma once

#include <bits/stdint-uintn.h>

#include <string>
#include <utility>
#include <vector>

class SourceManager {
   public:
    SourceManager() = default;

    /**
     * Loads a new source file into the SourceManager.
     *
     * This method reads the content of the file located at the specified path from the disk, splits
     * the content into lines, and stores the file metadata for future reference. It throws an
     * std::runtime_exception if the file cannot be opened.
     *
     * @param path The file system path of the source file to be loaded.
     * @return A pair where the first element is the unique identifier of the newly loaded source
     *         file, and the second element is a string view of its contents.
     */
    std::pair<int, std::string_view> load_new_source_file(std::string path);

    /**
     * Calculates the line and column number for a given offset in a specific source file.
     *
     * This method determines the line and column number corresponding to a byte offset within the
     * contents of a source file.
     *
     * @param fileID The identifier for the source file.
     * @param offset The byte offset within the source file for which the line
     *        and column should be calculated.
     * @return A pair of integers where the first element is the line number and
     *         the second element is the column number, 1-based.
     */
    [[nodiscard]] std::pair<int, int> get_line_column(int fileID, uint32_t offset) const;

    /**
     * Returns the file system path of the source file identified by the given file ID.
     *
     * This method retrieves the path of a previously loaded source file based on its unique
     * identifier. If the identifier is invalid or out of range, the behavior is undefined.
     *
     * @param fileID The unique identifier of the source file whose path is to be retrieved.
     * @return A string view representing the file system path of the specified source file.
     */
    [[nodiscard]] std::string_view get_source_file_path(const int fileID) const {
        return sourceFiles_[fileID].path();
    }

   private:
    class SourceFile {
       public:
        SourceFile(std::string path, std::string contents, std::vector<int> linesStarts)
            : path_(std::move(path)),
              contents_(std::move(contents)),
              linesStarts_(std::move(linesStarts)) {}

        [[nodiscard]] const std::string& path() const { return path_; }
        [[nodiscard]] const std::string& contents() const { return contents_; }
        [[nodiscard]] const std::vector<int>& lines_starts() const { return linesStarts_; }

       private:
        std::string path_, contents_;
        std::vector<int> linesStarts_;
    };
    std::vector<SourceFile> sourceFiles_;
};