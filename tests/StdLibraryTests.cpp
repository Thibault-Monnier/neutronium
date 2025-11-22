#include <gtest/gtest.h>

#include <string>

#include "common/Tester.hpp"

TEST_F(NeutroniumTester, PrintCharacter) {
    const std::string code = R"(
        extern fn print_c(char: int);

        fn main(): {
            let c = 65;  # ASCII value for 'A'
            print_c(c);
            print_c(10);  # Newline
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "A\n");
}

TEST_F(NeutroniumTester, PrintNumber) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        fn print_num_with_newline(num: int): {
            print_num(num);
            print_c(10);  # Newline
        }

        fn main(): {
            let num = 12345;
            print_num_with_newline(num);
            print_num_with_newline(-num);
            print_num_with_newline(-0);
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "12345\n-12345\n0\n");
}

TEST_F(NeutroniumTester, ModFunction) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        extern fn mod(a: int, b: int) -> int;

        fn main(): {
            let a = 10;
            let b = 3;
            print_num(mod(a, b));
            print_c(10);  # Newline
            print_num(mod(-a, b));
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "1\n-1");
}

TEST_F(NeutroniumTester, AbsFunction) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        extern fn abs(val: int) -> int;

        fn main(): {
            print_num(abs(-42));
            print_c(10);
            print_num(abs(42));
            print_c(10);
            print_num(abs(-0));
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "42\n42\n0");
}

TEST_F(NeutroniumTester, MinFunction) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        extern fn min(a: int, b: int) -> int;

        fn print_num_and_newline(num: int): {
            print_num(num);
            print_c(10);  # Newline
        }

        fn main(): {
            print_num_and_newline(min(10, 20)); # 10
            print_num_and_newline(min(20, 10)); # 10
            print_num_and_newline(min(10, 10)); # 10
            print_num_and_newline(min(-10, 20)); # -10
            print_num_and_newline(min(20, -10)); # -10
            print_num_and_newline(min(-10, -20)); # -20
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "10\n10\n10\n-10\n-10\n-20\n");
}

TEST_F(NeutroniumTester, MaxFunction) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        extern fn max(a: int, b: int) -> int;

        fn print_num_and_newline(num: int): {
            print_num(num);
            print_c(10);  # Newline
        }

        fn main(): {
            print_num_and_newline(max(10, 20)); # 20
            print_num_and_newline(max(20, 10)); # 20
            print_num_and_newline(max(10, 10)); # 10
            print_num_and_newline(max(-10, 20)); # 20
            print_num_and_newline(max(20, -10)); # 20
            print_num_and_newline(max(-10, -20)); # -10
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "20\n20\n10\n20\n20\n-10\n");
}

TEST_F(NeutroniumTester, PowFunction) {
    const std::string code = R"(
        extern fn print_num(num: int);
        extern fn print_c(char: int);

        extern fn pow(a: int, b: int) -> int;

        fn print_num_and_newline(num: int): {
            print_num(num);
            print_c(10);  # Newline
        }

        fn main(): {
            print_num_and_newline(pow(2, 3));  # 8
            print_num_and_newline(pow(5, 0));  # 1
            print_num_and_newline(pow(-2, 3)); # -8
            print_num_and_newline(pow(-2, 0)); # 1
        }
    )";
    const auto [exit, output] = runWithOutput(code);
    EXPECT_EQ(exit, 0);
    EXPECT_EQ(output, "8\n1\n-8\n1\n");
}