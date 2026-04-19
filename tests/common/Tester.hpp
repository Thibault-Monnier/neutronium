#pragma once

#include <gtest/gtest.h>
#include <unistd.h>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

class NeutroniumTester : public testing::Test {
   public:
    NeutroniumTester() = default;

    [[nodiscard]] std::string compileFail(const std::string& code) const {
        return compile(code, false);
    }

    [[nodiscard]] int run(const std::string& code) const {
        compile(code, true);
        chdir(originalCwd_.c_str());
        return WEXITSTATUS(std::system(outputBinary_.c_str()));
    }

    struct Output {
        int exit;
        std::string output;
    };

    [[nodiscard]] Output runWithOutput(const std::string& code) const {
        compile(code, true);

        chdir(originalCwd_.c_str());

        const std::string cmd = outputBinary_.string() + " 2>&1";
        FILE* pipe = popen(cmd.c_str(), "r");
        if (!pipe) {
            return {.exit = -1, .output = "Failed to run binary"};
        }
        std::array<char, 256> buffer;
        std::string result;
        while (fgets(buffer.data(), buffer.size(), pipe)) {
            result += buffer.data();
        }
        const int exitCode = WEXITSTATUS(pclose(pipe));

        return {.exit = exitCode, .output = result};
    }

   private:
    std::string compile(const std::string& code, bool shouldSucceed) const {
        // Write the source file
        {
            std::ofstream out(sourceFile_);
            out << code;
        }

        chdir(projectRoot_.c_str());

        const std::string errorFile = (projectRoot_ / "compile_error.log").string();
        const std::string errorFileIr = (projectRoot_ / "compile_error_ir.log").string();

        const std::string cmdBase = compiler_.string() + " -d " + sourceFile_.filename().string();
        const std::string cmd = cmdBase + " > /dev/null 2> " + errorFile;
        const std::string cmdWithIr = cmdBase + " --enable-ir" + " > /dev/null 2> " + errorFileIr;

        const int status = WEXITSTATUS(std::system(cmd.c_str()));
        const int irStatus = WEXITSTATUS(std::system(cmdWithIr.c_str()));

        std::ifstream err(errorFile);
        const std::string errorMsg((std::istreambuf_iterator(err)),
                                   std::istreambuf_iterator<char>());
        std::filesystem::remove(errorFile);

        std::ifstream errIr(errorFileIr);
        const std::string errorMsgIr((std::istreambuf_iterator(errIr)),
                                     std::istreambuf_iterator<char>());
        std::filesystem::remove(errorFileIr);

        if (shouldSucceed) {
            EXPECT_EQ(status, 0) << formatError("Compilation failed unexpectedly", errorMsg);
            EXPECT_EQ(irStatus, 0) << formatError("IR compilation failed unexpectedly", errorMsgIr);
        } else {
            EXPECT_NE(status, 0) << formatError("Compilation succeeded unexpectedly");
            EXPECT_NE(irStatus, 0) << formatError("IR compilation succeeded unexpectedly");
        }

        return errorMsg;
    }

    static std::string formatError(const std::string& error, const std::string& msg) {
        return "\033[1;31m" + error + ":" + "\033[0m\n" + msg;
    }

    static std::string formatError(const std::string& error) {
        return "\033[1;31m" + error + "\033[0m\n";
    }

   private:
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

        const auto* info = testing::UnitTest::GetInstance()->current_test_info();
        if (info->result()->Failed() && !capturedCerr_.str().empty()) {
            std::cerr << "\n[Compiler stderr]\n" << capturedCerr_.str() << "[/Compiler stderr]\n";
        }

        std::filesystem::remove(sourceFile_);
        chdir(originalCwd_.c_str());
    }
};
