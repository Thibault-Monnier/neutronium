#pragma once

#ifndef PROJECT_ROOT_DIR
#warning "PROJECT_ROOT_DIR not defined — using fallback for IDE only"
#define PROJECT_ROOT_DIR "."
#endif

#include <gtest/gtest.h>
#include <unistd.h>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

class NeutroniumTester : public ::testing::Test {
   protected:
    std::filesystem::path originalCwd_;
    std::filesystem::path projectRoot_;
    std::filesystem::path compiler_;
    std::filesystem::path sourceFile_;
    std::filesystem::path outputBinary_;

    void SetUp() override {
        using namespace std::filesystem;

        projectRoot_ = projectRoot_ = std::filesystem::path(PROJECT_ROOT_DIR);

        compiler_ = projectRoot_ / "build" / "neutronium";
        sourceFile_ = projectRoot_ / "tmp_test.nt";
        outputBinary_ = projectRoot_ / "neutro" / "out";
    }

    void TearDown() override {
        std::filesystem::remove(sourceFile_);
        chdir(originalCwd_.c_str());
    }

    [[nodiscard]] int compile(const std::string& code) const {
        // Write source file
        {
            std::ofstream out(sourceFile_);
            out << code;
        }

        chdir(projectRoot_.c_str());

        const std::string cmd =
            compiler_.string() + " " + sourceFile_.filename().string() + " > /dev/null 2>&1";
        return WEXITSTATUS(std::system(cmd.c_str()));
    }

    [[nodiscard]] int run(const std::string& code) const {
        const int compileStatus = compile(code);
        EXPECT_EQ(compileStatus, 0) << "Compilation failed unexpectedly";

        chdir(originalCwd_.c_str());

        const int runStatus = std::system(outputBinary_.c_str());
        return WEXITSTATUS(runStatus);
    }
};
