#pragma once

#include <gtest/gtest.h>
#include <unistd.h>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

class NeutroniumTester : public ::testing::Test {
   protected:
    std::streambuf* oldCerrBuf_;
    std::ostringstream capturedCerr_;

    std::filesystem::path originalCwd_;
    std::filesystem::path projectRoot_;
    std::filesystem::path compiler_;
    std::filesystem::path sourceFile_;
    std::filesystem::path outputBinary_;

    void SetUp() override {
        using namespace std::filesystem;
        originalCwd_ = current_path();
        projectRoot_ = std::filesystem::path(PROJECT_ROOT_DIR);
        compiler_ = projectRoot_ / "build" / "neutronium";
        sourceFile_ = projectRoot_ / "tmp_test.nt";
        outputBinary_ = projectRoot_ / "neutro" / "out";

        oldCerrBuf_ = std::cerr.rdbuf(capturedCerr_.rdbuf());
    }

    void TearDown() override {
        std::cerr.rdbuf(oldCerrBuf_);

        const auto* info = ::testing::UnitTest::GetInstance()->current_test_info();
        if (info->result()->Failed() && !capturedCerr_.str().empty()) {
            std::cerr << "\n[Compiler stderr]\n" << capturedCerr_.str() << "[/Compiler stderr]\n";
        }

        std::filesystem::remove(sourceFile_);
        chdir(originalCwd_.c_str());
    }

    [[nodiscard]] std::pair<int, std::string> compile(const std::string& code) const {
        // Write source file
        {
            std::ofstream out(sourceFile_);
            out << code;
        }

        chdir(projectRoot_.c_str());

        const std::string errorFile = (projectRoot_ / "compile_error.log").string();
        const std::string cmd = compiler_.string() + " " + sourceFile_.filename().string() +
                                " > /dev/null 2> " + errorFile;

        const int status = WEXITSTATUS(std::system(cmd.c_str()));

        std::ifstream err(errorFile);
        const std::string errorMsg((std::istreambuf_iterator<char>(err)),
                                   std::istreambuf_iterator<char>());
        std::filesystem::remove(errorFile);

        if (!errorMsg.empty()) {
            std::cerr << errorMsg;
            std::cerr.flush();
        }

        return {status, errorMsg};
    }

    [[nodiscard]] int run(const std::string& code) const {
        auto [compileStatus, compileErr] = compile(code);
        EXPECT_EQ(compileStatus, 0) << "Compilation failed unexpectedly:\n" << compileErr;
        chdir(originalCwd_.c_str());
        return WEXITSTATUS(std::system(outputBinary_.c_str()));
    }
};
