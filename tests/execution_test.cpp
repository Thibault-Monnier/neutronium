#include "common/tester.hpp"

TEST_F(NeutroniumTester, PrimeNumberCheck) {
    const std::string codeTemplate = R"(
        let integer = {val};
        let mut isPrime = true;
        let mut smallestDivisor = 1;
        fn computeIsPrime: {
            if integer <= 1: {
                exit 1;
            }

            let mut curr = 2;
            while curr < integer: {
                if (integer / curr) * curr == integer: {
                    isPrime = false;
                    smallestDivisor = curr;
                    curr = integer;
                } else: {
                    curr = curr + 1;
                }
            }
        }

        computeIsPrime();

        if !isPrime: {
            exit smallestDivisor;
        }

        exit 0;
    )";

    auto checkPrimeProgram = [&](int n, int expectedExit) {
        std::string code = codeTemplate;
        code.replace(code.find("{val}"), 5, std::to_string(n));
        EXPECT_EQ(run(code), expectedExit) << "Failed for integer = " << n;
    };

    checkPrimeProgram(7, 0);    // Prime
    checkPrimeProgram(9, 3);    // Not prime, smallest divisor = 3
    checkPrimeProgram(1, 1);    // < 2, exit early
    checkPrimeProgram(4, 2);    // Not prime, smallest divisor = 2
    checkPrimeProgram(17, 0);   // Prime
    checkPrimeProgram(127, 0);  // Prime
}

TEST_F(NeutroniumTester, MutableReassignmentSameType) {
    const std::string code = R"(
        let mut x = 42;
        x = x + 1;
        exit x;
    )";

    EXPECT_EQ(run(code), 43);
}

TEST_F(NeutroniumTester, OperatorPrecedence) {
    const std::string code = R"(
        # Expect 2 + (3 * 4) = 14
        exit 2 + 3 * 4 - 1 / 2;
    )";

    EXPECT_EQ(run(code), 14);
}

TEST_F(NeutroniumTester, ParenthesizedExpressions) {
    const std::string code = R"(
        # (2 + 3) * (4 + 1) = 5 * 5 = 25
        exit (2 + 3) * (4 + 1);
    )";

    EXPECT_EQ(run(code), 25);
}

TEST_F(NeutroniumTester, NestedFunctionsExecute) {
    const std::string code = R"(
        fn outer: {
            fn inner: { exit 7; }
            inner();
        }
        outer();
        exit 0;        # unreachable
    )";

    EXPECT_EQ(run(code), 7);
}

TEST_F(NeutroniumTester, WhileLoop) {
    const std::string code = R"(
        let mut i = 0;
        while i < 5: {
            i = i + 1;
        }
        exit i;
    )";

    EXPECT_EQ(run(code), 5);
}

TEST_F(NeutroniumTester, IfElifElse) {
    const std::string codeTemplate = R"(
        let x = {val};
        if x < 5: {
            exit 1;
        } elif x == 5: {
            exit 2;
        } elif x == 6: {
            exit 3;
        } else: {
            exit 4;
        }
    )";

    auto testWithX = [&](const int x, const int expectedExit) {
        std::string code = codeTemplate;
        code.replace(code.find("{val}"), 5, std::to_string(x));
        EXPECT_EQ(run(code), expectedExit);
    };

    testWithX(4, 1);  // x < 5 → exit 1
    testWithX(5, 2);  // x == 5 → exit 2
    testWithX(6, 3);  // x == 6 → exit 3
    testWithX(7, 4);  // x > 6 → exit 4
}

TEST_F(NeutroniumTester, LogicalNegation) {
    const std::string code = R"(
        let x = {val};
        if !x: {
            exit 1;
        } else: {
            exit 0;
        }
    )";

    auto testWithX = [&](const bool x, const int expectedExit) {
        std::string codeWithX = code;
        codeWithX.replace(codeWithX.find("{val}"), 5, x ? "true" : "false");
        EXPECT_EQ(run(codeWithX), expectedExit);
    };

    testWithX(true, 0);   // !true → exit 0
    testWithX(false, 1);  // !false → exit 1
}

TEST_F(NeutroniumTester, ExitFromNestedIfInsideLoop) {
    const std::string code = R"(
        let mut i = 0;
        while true: {
            if i == 3: {
                exit i;
            } else: {
                i = i + 1;
            }
        }
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, MultipleFunctionCalls) {
    const std::string code = R"(
        let mut x = 0;
        fn bump: { x = x + 1; }

        bump();
        bump();
        bump();
        exit x;
    )";

    EXPECT_EQ(run(code), 3);
}
