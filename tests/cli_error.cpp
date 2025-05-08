#include <gtest/gtest.h>

#include <cstdlib>
#include <string>

TEST(CliError, MissingSourceFilepath) {
    // The path to the CLI executable
    const std::string cmd = "./neutronium";  // assumes current dir is `build/`

    // Run without arguments. Expect error and non-zero exit code
    const int exitCode = std::system(cmd.c_str());

    // Extract real exit code (macOS/Linux)
    const int status = WEXITSTATUS(exitCode);

    // Expect non-zero exit (e.g., 1 for general failure)
    EXPECT_NE(status, 0) << "CLI did not fail when no arguments were provided";
}

TEST(CliErrorTest, InvalidSourceFilepath) {
    const std::string cmd = "./neutronium nonexistent_file.nt";

    // Run with invalid file path
    int exitCode = std::system(cmd.c_str());
    int status = WEXITSTATUS(exitCode);

    EXPECT_NE(status, 0) << "CLI did not fail on invalid file path";
}
