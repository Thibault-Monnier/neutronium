#pragma once

#include <string>
#include <utility>

class SourceLocation {
   public:
    SourceLocation(const int line, const int column, std::string filename)
        : line_(line), column_(column), filename_(std::move(filename)) {}

    [[nodiscard]] int line() const { return line_; }
    [[nodiscard]] int column() const { return column_; }
    [[nodiscard]] const std::string& filename() const { return filename_; }

   private:
    int line_;
    int column_;
    std::string filename_;
};