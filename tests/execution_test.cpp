#include "common/tester.hpp"

TEST_F(NeutroniumTester, PrimeNumberCheck) {
    const std::string codeTemplate = R"(
        let integer: int = {val};
        let mut isPrime: bool = true;
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

TEST_F(NeutroniumTester, TypeSpecifiers) {
    const std::string code = R"(
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
    )";

    EXPECT_EQ(run(code), 42);
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

TEST_F(NeutroniumTester, NestedFunctionsExecute2) {
    const std::string code = R"(
        fn outer: {
            let mut x = 10;
            fn inner: {
                x = x + 1;
            }
            inner();
            exit x;
        }
        outer();
    )";

    EXPECT_EQ(run(code), 11);
}

TEST_F(NeutroniumTester, WhileLoop) {
    const std::string code = R"(
        let mut i: int = 0;
        while i <= 5: {
            i = i + 1;
        }
        exit i;
    )";

    EXPECT_EQ(run(code), 6);
}

TEST_F(NeutroniumTester, WhileLoopWithBreak) {
    const std::string code = R"(
        let mut i = 0;
        while true: {
            if i == 5: {
                break;
            }
            i = i + 1;
        }
        exit i;
    )";

    EXPECT_EQ(run(code), 5);
}

TEST_F(NeutroniumTester, WhileLoopWithContinue) {
    const std::string code = R"(
        let mut i = 0;
        let mut j = 0;
        while i < 5: {
            i = i + 1;
            if i >= 3: {
                continue;
            }
            j = j + i;
        }
        exit j;
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, IfElifElse) {
    const std::string codeTemplate = R"(
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

TEST_F(NeutroniumTester, LogicalNegation) {
    const std::string code = R"(
        let x: bool = {val};
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

TEST_F(NeutroniumTester, UnaryOperators) {
    const std::string code = R"(
        let mut x = -(-42);  # should be 42
        let mut y = +(+1);   # should be 1
        let mut z = !false;  # should be true → exit 1
        if z: {
            exit x / y;      # should be 42
        }
        exit 0;
    )";

    EXPECT_EQ(run(code), 42);
}

TEST_F(NeutroniumTester, CommentsAreIgnored) {
    const std::string code = R"(
        let x = 1;   # this is a comment
        let y = 2;   # another one
        exit x + y; # should be 3
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, SubtractionAssociativity) {
    const std::string code = R"( exit 10 - 3 - 2; )";
    EXPECT_EQ(run(code), 5);
}

TEST_F(NeutroniumTester, DivisionByZeroRuntimeNonZeroExit) {
    const std::string code = R"(
        exit 1 / 0;           # undefined behaviour → SIGFPE on most back-ends
    )";

    EXPECT_NE(run(code), 0);
}

TEST_F(NeutroniumTester, WhileFalseSkipsBody) {
    const std::string code = R"(
        let mut i = 0;
        while false: {
            i = i + 1;        # should never run
        }
        exit i;               # expect 0
    )";
    EXPECT_EQ(run(code), 0);
}

TEST_F(NeutroniumTester, MultiplyWithUnaryLiteral) {
    const std::string code = R"( exit 2 * -3; )";
    EXPECT_EQ(run(code), 250);  // -6 → 250 in 8-bit unsigned
}

TEST_F(NeutroniumTester, ScopeDeclaration) {
    const std::string code = R"(
        {
            let x = 2;
            let mut y = 3;
            {
                let z = 4;
                y = y + z;      # y = 3 + 4 = 7
            }
            exit x + y;  # = 2 + 7 = 9
        }
    )";

    EXPECT_EQ(run(code), 9);
}

TEST_F(NeutroniumTester, NestedControlFlow) {
    const std::string code = R"(
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
        1;
        true;

        {
            !(1 == 1 - 0);
            2 + 3;

            fn x: {}
            x();
        }

        exit 0;
    )";
    EXPECT_EQ(run(code), 0);
}

TEST_F(NeutroniumTester, FunctionCalls) {
    const std::string code = R"(
        let mut var = 3545654;

        fn inc: {
            var = var + 1;
        }

        fn dec: {
            fn minusOne: {
                var = var - 1;
            }
            minusOne();
        }

        fn setToZero: {
            if (var != 0): {
                var = 0;
            }
        }

        setToZero();
        inc();
        inc();
        dec();
        inc();

        exit var;
    )";
    EXPECT_EQ(run(code), 2);
}