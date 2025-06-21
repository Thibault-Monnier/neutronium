#include <gtest/gtest.h>

#include <array>
#include <cstdio>
#include <fstream>
#include <string>

const std::string projectRoot = PROJECT_ROOT_DIR;
const std::string neutroniumPath = projectRoot + "/build/neutronium";

namespace {

int run_and_capture(const std::string& cmd, std::string& out) {
    FILE* pipe = popen((cmd + " 2>&1").c_str(), "r");
    if (!pipe) return -1;

    std::array<char, 256> buffer{};
    out.clear();
    while (fgets(buffer.data(), buffer.size(), pipe)) {
        out += buffer.data();
    }

    const int rc = pclose(pipe);
    if (WIFEXITED(rc)) return WEXITSTATUS(rc);
    return -1;
}

}  // namespace

TEST(CliErrorTest, MissingSourceFilepath) {
    std::string output;
    const int status = run_and_capture(neutroniumPath, output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Missing input file")) << output;
}

TEST(CliErrorTest, InvalidSourceFilepath) {
    std::string output;
    const int status = run_and_capture(neutroniumPath + " nonexistent_file.nt", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Error:")) << output;
}

TEST(CLIErrorTest, InvalidTargetType) {
    std::string output;
    const int status = run_and_capture(neutroniumPath + " --target-type=invalid", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Unknown target type")) << output;
}

TEST(CLITargetTest, LibraryTarget) {
    const std::string tempFile = projectRoot + "/temp_test.nt";
    {
        std::ofstream out(tempFile);
        out << "fn abc(): { exit 0; }\n";  // No main function is valid
    }

    std::string output;
    int status = run_and_capture(neutroniumPath + " --target-type=library " + tempFile, output);
    EXPECT_TRUE(status == 0 || output.contains("Assembling"))
        << output;  // Ensure it compiles successfully

    {
        std::ofstream out(tempFile);
        out << "fn main(): { exit 0; }\n";  // Adding main function should fail
    }

    status = run_and_capture(neutroniumPath + " --target-type=library " + tempFile, output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("main") && output.contains("library target")) << output;
}

TEST(CLITargetTest, ExecutableTarget) {
    const std::string tempFile = projectRoot + "/temp_test.nt";
    {
        std::ofstream out(tempFile);
        out << "fn abc(): { exit 0; }\n";  // No main function should fail
    }

    std::string output;
    int status = run_and_capture(neutroniumPath + " --target-type=executable " + tempFile, output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("main") && output.contains("executable target")) << output;

    {
        std::ofstream out(tempFile);
        out << "fn main(): { exit 0; }\n";  // Adding main function should succeed
    }

    status = run_and_capture(neutroniumPath + " --target-type=executable " + tempFile, output);
    EXPECT_TRUE(status == 0 || output.contains("Assembling"))
        << output;  // Ensure it compiles successfully
}

TEST(CliErrorTest, InvalidLogType) {
    std::string output;
    const int status = run_and_capture(neutroniumPath + " --log=invalid", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Unknown log type")) << output;
}

TEST(CliNonErrorTest, HelpOption) {
    std::string output;
    const int status = run_and_capture(neutroniumPath + " --help", output);
    EXPECT_EQ(status, 0);
    EXPECT_TRUE(output.contains("Usage:")) << output;
}

TEST(CliNonErrorTest, ValidMinimalSourceFile) {
    const std::string tempFile = projectRoot + "/temp_test.nt";
    {
        std::ofstream out(tempFile);
        out << "fn main(): { exit 0; }\n";
    }

    std::string output;
    const int status = run_and_capture(neutroniumPath + " " + tempFile, output);
    EXPECT_EQ(status, 0) << output;

    std::remove(tempFile.c_str());
}

TEST(CliErrorTest, InvalidOption) {
    std::string output;
    const int status = run_and_capture(neutroniumPath + " --invalid-option", output);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(output.contains("Error") && output.contains("does not exist")) << output;
}

TEST(CliNonErrorTest, LogArguments) {
    const std::string tempFile = projectRoot + "/temp_test.nt";
    {
        std::ofstream out(tempFile);
        out << "fn main(): { exit 0; }\n";
    }

    std::string output;

    const std::string baseCompileCommand = neutroniumPath + " " + tempFile;

    int status = run_and_capture(baseCompileCommand + " --log-code", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("fn main():")) << output;

    status = run_and_capture(baseCompileCommand + " --log-tokens", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("EOF_:")) << output;

    status = run_and_capture(baseCompileCommand + " --log-ast", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("Program")) << output;

    status = run_and_capture(baseCompileCommand + " --log-assembly", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("_start:")) << output;

    status = run_and_capture(baseCompileCommand + " -d", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("fn main():") && output.contains("EOF_:") &&
                output.contains("Program") && output.contains("_start:"))
        << output;

    status = run_and_capture(baseCompileCommand + " --log=code,ast", output);
    EXPECT_EQ(status, 0) << output;
    EXPECT_TRUE(output.contains("fn main():") && output.contains("Program")) << output;

    std::remove(tempFile.c_str());
}