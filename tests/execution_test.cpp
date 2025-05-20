#include "common/tester.hpp"

TEST_F(NeutroniumTester, PrimeNumberCheck) {
    const std::string codeTemplate = R"(
        fn computeIsPrime(integer: int) -> int: {
            if integer <= 1: {
                return 1;
            }

            let mut curr = 2;
            while curr * curr <= integer: {
                if (integer / curr) * curr == integer: {
                    return curr;
                }

                curr = curr + 1;
            }

            return 0;
        }

        fn main(): {
            let testValue: int = {val};

            let smallestDiv = computeIsPrime(testValue);

            if smallestDiv != 0: {
                exit smallestDiv;
            }

            exit 0;
        }
    )";

    auto checkPrimeProgram = [&](const long long n, const int expectedExit) {
        std::string code = codeTemplate;
        code.replace(code.find("{val}"), 5, std::to_string(n));
        EXPECT_EQ(run(code), expectedExit) << "Failed for integer = " << n;
    };

    checkPrimeProgram(7, 0);              // Prime
    checkPrimeProgram(9, 3);              // Not prime, smallest divisor = 3
    checkPrimeProgram(1, 1);              // < 2, exit early
    checkPrimeProgram(4, 2);              // Not prime, smallest divisor = 2
    checkPrimeProgram(17, 0);             // Prime
    checkPrimeProgram(127, 0);            // Prime
    checkPrimeProgram(1000000000039, 0);  // Prime
}

TEST_F(NeutroniumTester, MutableReassignment) {
    const std::string code = R"(
        fn main(): {
            let mut x = 42;
            x = x + 1;
            exit x;
        }
    )";

    EXPECT_EQ(run(code), 43);
}

TEST_F(NeutroniumTester, TypeSpecifiers) {
    const std::string code = R"(
        fn main(): {
            let x: int = 42;
            let mut y: int = 0;

            let z: bool = true;
            let mut w: bool = false;

            if z: {
                y = x + 1; # y = 43
                w = !w;
            }
            if w: {
                y = y + 1; # y = 44
            }

            exit x;
        }
    )";

    EXPECT_EQ(run(code), 42);
}

TEST_F(NeutroniumTester, FunctionReturnValues) {
    /* 1. Two independent return paths */
    const std::string codeAbs = R"(
        fn absValue(val: int) -> int: {
            if val < 0: {
                return -val;
            }
            return val;
        }

        fn main(): {
            exit absValue(-42);
        }
    )";
    EXPECT_EQ(run(codeAbs), 42);

    /* 2. Returning a boolean and using it in the caller */
    const std::string codeIsEven = R"(
        fn isEven(x: int) -> bool: {
            return (x / 2) * 2 == x;
        }

        fn main(): {
            if isEven(6): {
                exit 1;
            } else: {
                exit 0;
            }
        }
    )";
    EXPECT_EQ(run(codeIsEven), 1);

    /* 3. Early return from inside a loop */
    const std::string codeFind = R"(
        fn findFirstGtTen() -> int: {
            let mut i = 0;
            while true: {
                if i > 10: {
                    return i;
                }
                i = i + 1;
            }
        }

        fn main(): {
            exit findFirstGtTen();
        }
    )";
    EXPECT_EQ(run(codeFind), 11);
}

TEST_F(NeutroniumTester, ExpressionsEvaluation) {
    const std::string code = R"(
        fn main(): {
            let mut x = -(-42);  # should be 42
            let mut y = +1;      # should be 1
            let mut z = !false;  # should be true
            if z: {
                exit x / y;      # should be 42
            }
            exit 0;
        }
    )";
    EXPECT_EQ(run(code), 42);

    const std::string code1 = R"(
        fn main(): {
            # Expect 2 + (3 * 4) = 14
            exit 2 + 3 * 4 - 1 / 2;
        }
    )";
    EXPECT_EQ(run(code1), 14);

    const std::string code2 = R"(
        fn main(): {
            # (2 + 3) * (4 + 1) = 5 * 5 = 25
            exit (2 + 3) * (4 + 1);
        }
    )";
    EXPECT_EQ(run(code2), 25);

    const std::string code3 = R"( fn main(): { exit 10 - 3 - 2; } )";
    EXPECT_EQ(run(code3), 5);

    const std::string code4 = R"( fn main(): { exit 2 * -3; } )";
    EXPECT_EQ(run(code4), 250);  // -6 → 250 in 8-bit unsigned
}

TEST_F(NeutroniumTester, FunctionWithParameters) {
    const std::string code = R"(
        fn add(a: int, b: int): {
            exit a + b;
        }
        fn main(): {
            add(-2, 5);
        }
    )";
    EXPECT_EQ(run(code), 3);

    const std::string code2 = R"(
        fn multiplyOrAdd(a: int, b: int, shouldAdd: bool): {
            if shouldAdd: {
                exit a + b;
            } else: {
                exit a * b;
            }
        }

        fn main(): {
            let shouldMultiply = {val};
            multiplyOrAdd((1 + 2), 4 * 1 - 0, (!shouldMultiply));
        }
    )";
    auto testWithShouldAdd = [&](const bool shouldMultiply, const int expectedResult) {
        std::string codeWithShouldAdd = code2;
        codeWithShouldAdd.replace(codeWithShouldAdd.find("{val}"), 5,
                                  shouldMultiply ? "true" : "false");
        EXPECT_EQ(run(codeWithShouldAdd), expectedResult);
    };
    testWithShouldAdd(false, 7);  // shouldAdd = true → 3 + 4 = 7
    testWithShouldAdd(true, 12);  // shouldAdd = false → 3 * 4 = 12

    const std::string code3 = R"(
        fn inc(mut x: int): {
            x = x + 1;
            exit x;
        }
        fn main(): { inc(5); }
    )";
    EXPECT_EQ(run(code3), 6);
}

TEST_F(NeutroniumTester, FunctionCalls) {
    const std::string code = R"(
        fn inc(mut var2: int) -> int: {
            var2 = var2 + 1;
            return var2;
        }

        fn dec(mut var3: int) -> int: {
            var3 = var3 - 1;
            return var3;
        }

        fn setToZero(mut var4: int) -> int: {
            if (var4 != 0): {
                var4 = 0;
            }
            return var4;
        }

        fn main(): {
            let mut var = 3545654;

            var = setToZero(var);
            var = inc(var);
            var = inc(var);
            var = dec(var);
            var = inc(var);

            exit var;
        }
    )";
    EXPECT_EQ(run(code), 2);
}

TEST_F(NeutroniumTester, WhileLoop) {
    const std::string code = R"(
        fn main(): {
            let mut i: int = 0;
            while i <= 5: {
                i = i + 1;
            }
            exit i;
        }
    )";

    EXPECT_EQ(run(code), 6);
}

TEST_F(NeutroniumTester, WhileLoopWithContinueAndBreak) {
    const std::string code = R"(
        fn main(): {
            let mut i = 0;
            let mut j = 0;
            while true: {
                if i == 5: {
                    break;
                }
                i = i + 1;
                if i >= 3: {
                    continue;
                }
                j = j + i;
            }
            exit j;
        }
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, IfElifElse) {
    const std::string codeTemplate = R"(
        fn main(): {
            let mut x = {val};
            if x < 5: {
                exit 1;
            } elif x == 5: {
                exit 2;
            } elif x == 6: {
                while x < 10: {
                    x = x + 1;
                }
            } else: {
                exit 4;
            }
            exit x;
        }
    )";

    auto testWithX = [&](const int x, const int expectedExit) {
        std::string code = codeTemplate;
        code.replace(code.find("{val}"), 5, std::to_string(x));
        EXPECT_EQ(run(code), expectedExit);
    };

    testWithX(4, 1);   // x < 5 → exit 1
    testWithX(5, 2);   // x == 5 → exit 2
    testWithX(6, 10);  // x == 6 → exit 10
    testWithX(7, 4);   // x > 6 → exit 4
}

TEST_F(NeutroniumTester, ExitFromNestedIfInsideLoop) {
    const std::string code = R"(
        fn main(): {
            let mut i = 0;
            while true: {
                if i == 3: {
                    exit i;
                } else: {
                    i = i + 1;
                }
            }
        }
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, CommentsAreIgnored) {
    const std::string code = R"(
        fn main(): {
            let x = 1;   # this is a comment
            exit x; # should be 1
        }
    )";

    EXPECT_EQ(run(code), 1);
}

TEST_F(NeutroniumTester, DivisionByZeroRuntimeNonZeroExit) {
    const std::string code = R"(
        fn main(): {
            exit 1 / 0;           # undefined behaviour → SIGFPE on most back-ends
        }
    )";

    EXPECT_NE(run(code), 0);
}

TEST_F(NeutroniumTester, WhileFalseSkipsBody) {
    const std::string code = R"(
        fn main(): {
            let mut i = 0;
            while false: {
                i = i + 1;        # should never run
            }
            exit i;               # expect 0
        }
    )";
    EXPECT_EQ(run(code), 0);
}

TEST_F(NeutroniumTester, ScopeDeclaration) {
    const std::string code = R"(
        fn main(): {
            {
                let x = 2;
                let mut y = 3;
                {
                    let z = 4;
                    y = y + z;      # y = 3 + 4 = 7
                }
                exit x + y;  # = 2 + 7 = 9
            }
        }
    )";

    EXPECT_EQ(run(code), 9);
}

TEST_F(NeutroniumTester, NestedControlFlow) {
    const std::string code = R"(
        fn main(): {
            let mut condition = {val};
            let mut exitCode: int = - 2*128;
            if condition: {
                exitCode = 1;
                while condition: {
                    condition = !condition;
                    exitCode = 0;
                }
            } else: {
                if exitCode == 0: {
                    exitCode = 2;
                } else: {
                    exitCode = 3;
                }
            }

            exit exitCode;
        }
    )";

    auto testWithCondition = [&](const bool condition, const int expectedExit) {
        std::string codeWithCondition = code;
        codeWithCondition.replace(codeWithCondition.find("{val}"), 5, condition ? "true" : "false");
        EXPECT_EQ(run(codeWithCondition), expectedExit);
    };

    testWithCondition(true, 0);   // condition = true → exitCode = 0
    testWithCondition(false, 3);  // condition = false → exitCode = 3
}

TEST_F(NeutroniumTester, ExpressionStatements) {
    const std::string code = R"(
        fn x(): {}

        fn main(): {
            1;
            true;

            {
                !(1 == 1 - 0);
                2 + 3;

                x();
            }

            exit 0;
        }
    )";
    EXPECT_EQ(run(code), 0);
}