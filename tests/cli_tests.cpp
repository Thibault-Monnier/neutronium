#include <gtest/gtest.h>

#include <array>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <string>

const std::string project_root = PROJECT_ROOT_DIR;
const std::string neutronium_path = project_root + "/build/neutronium";

// Same helper as before
static int run_and_capture(const std::string& cmd, std::string& out) {
    FILE* pipe = popen((cmd + " 2>&1").c_str(), "r");
    if (!pipe) return -1;

    std::array<char, 256> buffer{};
    out.clear();
    while (fgets(buffer.data(), buffer.size(), pipe)) {
        out += buffer.data();
    }

    int rc = pclose(pipe);
#if defined(_WIN32)
    return rc;
#else
    if (WIFEXITED(rc)) return WEXITSTATUS(rc);
    return -1;
#endif
}

TEST(CliErrorTest, MissingSourceFilepath) {
    std::string output;
    int status = run_and_capture(neutronium_path, output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Missing input file")) << output;
}

TEST(CliErrorTest, InvalidSourceFilepath) {
    std::string output;
    int status = run_and_capture(neutronium_path + " nonexistent_file.nt", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Error:")) << output;
}

TEST(CliErrorTest, InvalidLogType) {
    std::string output;
    int status = run_and_capture(neutronium_path + " --log=invalid", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Unknown log type")) << output;
}

TEST(CliNonErrorTest, HelpOption) {
    std::string output;
    int status = run_and_capture(neutronium_path + " --help", output);
    EXPECT_EQ(status, 0);
    EXPECT_TRUE(output.contains("Usage:")) << output;
}

TEST(CliNonErrorTest, ValidMinimalSourceFile) {
    const std::string tempFile = project_root + "/temp_test.nt";
    {
        std::ofstream out(tempFile);
        out << "fn main(): { exit 0; }\n";
    }

    std::string output;
    int status = run_and_capture(neutronium_path + " " + tempFile, output);
    EXPECT_EQ(status, 0) << output;

    std::remove(tempFile.c_str());
}
