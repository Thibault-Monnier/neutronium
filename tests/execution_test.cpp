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

                curr += 1;
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

TEST_F(NeutroniumTester, RecursiveFibonacci) {
    const std::string code = R"(
        fn fibonacci(n: int) -> int: {
            if n <= 1: {
                return n;
            }
            return fibonacci(n - 1) + fibonacci(n - 2);
        }

        fn main(): {
            let n = {val};
            let result = fibonacci(n);
            exit result;
        }
    )";
    auto testFibonacci = [&](const int n, const int expectedResult) {
        std::string codeWithN = code;
        codeWithN.replace(codeWithN.find("{val}"), 5, std::to_string(n));
        EXPECT_EQ(run(codeWithN), expectedResult) << "Failed for n = " << n;
    };

    testFibonacci(0, 0);
    testFibonacci(1, 1);
    testFibonacci(2, 1);
    testFibonacci(3, 2);
    testFibonacci(7, 13);
    testFibonacci(10, 55);
    testFibonacci(35, 9227465 % 256);  // 9227465 % 256 = 201
}

TEST_F(NeutroniumTester, FastIterativeFibonacci) {
    const std::string code = R"(
        fn computeFibonacci(mut n: int) -> int: {
            let mut a = 0;
            let mut b = 1;
            while n > 0: {
                let temp = a;
                a = b;
                b += temp;
                n -= 1;
            }
            return a;
        }

        fn main(): {
            let n = {val};
            exit computeFibonacci(n);
        }
    )";
    auto testFastFibonacci = [&](const int n, const int expectedResult) {
        std::string codeWithN = code;
        codeWithN.replace(codeWithN.find("{val}"), 5, std::to_string(n));
        EXPECT_EQ(run(codeWithN), expectedResult) << "Failed for n = " << n;
    };

    testFastFibonacci(0, 0);
    testFastFibonacci(1, 1);
    testFastFibonacci(10, 55);
    testFastFibonacci(99, 2);  // 218,922,995,834,555,169,026 mod 256 = 2
}

TEST_F(NeutroniumTester, MutableReassignment) {
    const std::string code = R"(
        fn main(): {
            let mut x = 42;
            x += 1;
            exit x;
        }
    )";

    EXPECT_EQ(run(code), 43);
}

TEST_F(NeutroniumTester, CompoundAssignments) {
    const std::string code = R"(
        fn main(): {
            let mut x = 10;
            x += 5;  # x = 15
            x -= 3;  # x = 12
            x *= 2;  # x = 24
            x /= 4;  # x = 6

            let mut y = 3;
            {
                y *= 2;  # y = 6
                let arr = [1, 2, 3, 4, 5];
                y += arr[2];  # y = 6 + 3 = 9
                y -= x / (arr[1] + 0);  # y = 9 - 6 / 2 = 6
            }

            if (x == y): {
                exit x;  # should exit with 6
            } else: {
                exit 0;  # should not reach here
            }
        }
    )";
    EXPECT_EQ(run(code), 6);
}

TEST_F(NeutroniumTester, TypeSpecifiers) {
    const std::string code = R"(
        fn main(): {
            let x: int = 42;
            let mut y: int = 0;

            let mut a: int8 = 100;
            let b: int8 = 27 + -1;

            let z: bool = true;
            let mut w: bool = false;

            if z: {
                y = x + 1; # y = 43
                a += +b;  # a = 127
                w = !w;
            }
            if w: {
                y += 1; # y = 44
            }

            exit x;
        }
    )";

    EXPECT_EQ(run(code), 42);
}

TEST_F(NeutroniumTester, StandardLibrary) {
    const std::string code = R"(
        extern fn print_c(char: int);
        extern fn print_num(num: int);

        extern fn mod(a: int, b: int) -> int;

        fn print_mod(a: int, b: int): {
            let result = mod(a, b);
            print_num(result);
            print_c(10);
        }

        fn main(): {
            print_c(72);
            print_c(101);
            print_c(108);
            print_c(108);
            print_c(111);
            print_c(44);
            print_c(32);
            print_c(87);
            print_c(111);
            print_c(114);
            print_c(108);
            print_c(100);
            print_c(33);
            print_c(10);

            print_num(42);
            print_c(10);
            print_c(10);

            print_mod(654, 10);  # should print 4
        }
    )";
    const auto result = run_with_output(code);
    EXPECT_EQ(result.exit, 0);
    EXPECT_EQ(result.output, "Hello, World!\n42\n\n4\n");
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
                i += 1;
            }
            exit 1; # unreachable
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

    const std::string codeFunctions = R"(
        fn n(num: int) -> int: {
            return num;
        }

        fn main(): {
            let mut x = n(1) + n(2) / n(1); # should be 3
            x = x + n(3 * 1) * n(1 + 1); # should be 9
            exit x;
        }
    )";
    EXPECT_EQ(run(codeFunctions), 9);
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
        fn inc(mut var: int) -> int: {
            var -= -1;
            return var;
        }

        fn dec(mut var: int) -> int: {
            var -= 1;
            return var;
        }

        fn setToZero(mut var: int) -> int: {
            if (var != 0): {
                var = 0;
            }
            return var;
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

    const std::string code2 = R"(
        fn aPlusB(a: int, b: int) -> int: {
            return a + b;
        }

        fn aMinusB(a: int, b: int) -> int: {
            return a - b;
        }

        fn main(): {
            let a = 5;
            let b = 3;
            let c = aPlusB(a, b);
            let d = aMinusB(a, b);
            exit c * d; # a² - b²
        }
    )";
    EXPECT_EQ(run(code2), 16);
}

TEST_F(NeutroniumTester, WhileLoop) {
    const std::string code = R"(
        fn main(): {
            let mut i: int = 0;
            while i <= 5: {
                i += 1;
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
                i += 1;
                if i >= 3: {
                    continue;
                }
                j += i;
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

    const std::string noEndingElse = R"(
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
            }
            exit x;
        }
    )";

    auto testWithXNoElse = [&](const int x, const int expectedExit) {
        std::string code = noEndingElse;
        code.replace(code.find("{val}"), 5, std::to_string(x));
        EXPECT_EQ(run(code), expectedExit);
    };

    testWithXNoElse(4, 1);   // x < 5 → exit 1
    testWithXNoElse(5, 2);   // x == 5 → exit 2
    testWithXNoElse(6, 10);  // x == 6 → exit 10
    testWithXNoElse(7, 7);   // x > 6 → exit x
}

TEST_F(NeutroniumTester, ExitFromNestedIfInsideLoop) {
    const std::string code = R"(
        fn main(): {
            let mut i = 0;
            while true: {
                if i == 3: {
                    exit i;
                } else: {
                    i += 1;
                }
            }
        }
    )";

    EXPECT_EQ(run(code), 3);
}

TEST_F(NeutroniumTester, WhileFalseSkipsBody) {
    const std::string code = R"(
        fn main(): {
            let mut i = 0;
            while false: {
                i += 1;        # should never run
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
                    y += z;      # y = 3 + 4 = 7
                }
                exit x + y;  # = 2 + 7 = 9
            }
        }
    )";
    EXPECT_EQ(run(code), 9);

    const std::string code2 = R"(
        fn main(): {
            let x = 2;
            let mut y = 3;
            {
                let z = 4;
                y += z;      # y = 3 + 4 = 7
            }
            let z = 5;
            exit x + y + z;  # = 2 + 7 + 5 = 14
        }
    )";
    EXPECT_EQ(run(code2), 14);
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

TEST_F(NeutroniumTester, Arrays) {
    const std::string code = R"(
        fn main(): {
            # initialize with literal and read an element
            let arr = [10, 20, 30, 40, 50];
            exit arr[2];   # should be 30
        }
    )";
    EXPECT_EQ(run(code), 30);

    const std::string code2 = R"(
        fn main(): {
            # mutable array; update elements and reassign
            let mut arr: [int; 4] = [0, 0, 0, 0];
            arr[0] = 7;
            arr[3] = arr[0] + 1;
            exit arr[3];
        }
    )";
    EXPECT_EQ(run(code2), 8);

    const std::string code3 = R"(
        fn sum(arr: [int; 3]) -> int: {
            return arr[0] + arr[1] + arr[2];
        }

        fn main(): {
            let a = [5, 6, 7];
            exit sum(a);
        }
    )";
    EXPECT_EQ(run(code3), 18);

    const std::string code4 = R"(
        fn getArray() -> [int; 3]: {
            return [1, 2, 3];
        }

        fn main(): {
            let mut arr = getArray();
            arr[1] *= arr[0];
            exit arr[1];  # should be 2
        }
    )";
    EXPECT_EQ(run(code4), 2);

    const std::string code2dArray = R"(
        fn getArray() -> [[int; 2]; 2]: {
            return [[1, 2], [3, 4]];
        }

        fn main(): {
            let mut arr = getArray();
            arr[0][1] = arr[1][0] + arr[0][0];  # arr[0][1] = 3 + 1 = 4
            exit arr[0][1];  # should be 4
        }
    )";
    EXPECT_EQ(run(code2dArray), 4);

    const std::string codeArrayLiteralAccess = R"(
        fn main(): {
            let val = [1, 2, 3, 4, 5][2];
            exit val;  # Should be 3
        }
    )";
    EXPECT_EQ(run(codeArrayLiteralAccess), 3);

    const std::string codeTestTypeInference = R"(
        fn takesInt8(a: int8): { }

        fn main(): {
            let n = 13;
            takesInt8(n);

            let a: [[int8; 1]; 2] = [[n], [n + 1]];
            let b = a[0][0];

            exit a[1][0];
        }
    )";
    EXPECT_EQ(run(codeTestTypeInference), 14);
}

TEST_F(NeutroniumTester, ArrayIndexWithInt16) {
    const std::string code = R"(
        fn main(): {
            let arr = [10, 20, 30, 40];
            let idx: int16 = 2;
            exit arr[idx];  # should be 30
        }
    )";
    EXPECT_EQ(run(code), 30);
}

TEST_F(NeutroniumTester, ArrayElementTypeBubblesUp) {
    const std::string code = R"(
        fn forceInt16(unused: bool, x: int16) -> int16: { return x; }

        fn main(): {
            let k: int16 = 10;
            let arr = [[1, 2], [k, 4]];
            let control = forceInt16(true, 2);
            exit (arr[0][1] / control)  # asserts that arr is inferred as [[int16; 2]; 2]
                 + arr[1][0];           # 2/2 + 10 = 11
        }
    )";
    EXPECT_EQ(run(code), 11);
}
