#include "common/tester.hpp"

TEST_F(NeutroniumTester, ImmutableReassignmentFails) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            x = 2;          # illegal: x is immutable
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("immutable") &&
                (error.contains("assignment") || error.contains("Assignment")) &&
                error.contains("x"));

    const std::string code2 = R"(
        fn foo(x: int): {
            x = 2;      # illegal: x is immutable
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("immutable") &&
                (error2.contains("assignment") || error2.contains("Assignment")) &&
                error2.contains("x"));
}

TEST_F(NeutroniumTester, ReassignmentDifferentTypeFails) {
    const std::string code = R"(
        fn main(): {
            let mut x: int = 1;
            x = true;      # illegal: integer -> boolean
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        fn main(): {
            let mut x: bool = true;
            x = 1;       # illegal: boolean -> integer
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));

    const std::string code3 = R"(
        fn main(): {
            let mut x: int = 1;
            let y: bool = true;
            x = y;       # illegal: int -> bool
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type mismatch"));

    const std::string code4 = R"(
        fn foo(mut x: bool): {
            x = 1;      # illegal: bool -> int
        }
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("Type mismatch") && error4.contains("x"));
}

TEST_F(NeutroniumTester, ReassignmentDifferentInferredTypeFails) {
    const std::string code = R"(
        fn main(): {
            let mut flag = true;
            flag = 1;       # illegal: boolean -> integer
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        fn main(): {
            let mut x = 1;
            x = true;      # illegal: integer -> boolean
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));

    const std::string code3 = R"(
        fn main(): {
            let mut x = false;
            let y = 1;
            x = y;       # illegal: bool -> int
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type mismatch"));
}

TEST_F(NeutroniumTester, WrongSpecifiedTypeFails) {
    const std::string code = R"(
        fn main(): {
            let x: int = true != false;  # illegal: bool -> int
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("bool") && error.contains("int") && error.contains("type") &&
                error.contains("x"));

    const std::string code2 = R"(
        fn main(): {
            let mut x: bool = 1 * -5;   # illegal: int -> bool
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("int") && error2.contains("bool") && error2.contains("type") &&
                error2.contains("x"));
}

TEST_F(NeutroniumTester, ExitWithBooleanExpressionFails) {
    const std::string code = R"(
        fn main(): {
            let ok = true;
            exit ok;         # exit expects integer expression
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("exit") && error.contains("integer") && error.contains("type") &&
                error.contains("boolean"));
}

TEST_F(NeutroniumTester, TypeMismatchInExpressionFails) {
    const std::string code = R"(
        fn main(): {
            let a = 1;
            let b = true;
            let c = a + b;      # int + bool is illegal
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        fn main(): {
            let a = 1;
            let b = true;
            let c = a == b;     # int == bool is illegal
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));
}

TEST_F(NeutroniumTester, RedeclarationOfVariableError) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            let x = 2;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("Redeclaration") || error.contains("redeclaration")) &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("x"));
}

TEST_F(NeutroniumTester, UndeclaredVariableError) {
    const std::string code = R"(
        fn main(): { let x = y; }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && (error.contains("variable") || error.contains("symbol")) && error.contains("y"));

    const std::string code2 = R"(
        fn main(): { x = 1; }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("undeclared") && error2.contains("variable") &&
                error2.contains("x"));
}

TEST_F(NeutroniumTester, VariableUsedBeforeDeclarationError) {
    const std::string code = R"(
        fn main(): {
            let y = x + 1;
            let x = 1;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && (error.contains("variable") || error.contains("symbol")) && error.contains("x"));
}

TEST_F(NeutroniumTester, VariableOfTypeVoidFails) {
    const std::string code = R"(
        fn x(): {}

        fn main(): {
            let y = x();
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("void") && error.contains("type") && error.contains("y"));
}

TEST_F(NeutroniumTester, SymbolShadowingError) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            {
                let x = 2;
            }
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("Redeclaration") || error.contains("redeclaration")) &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("x"));

    const std::string code2 = R"(
        fn main(): {
            {
                let y = 10;
            }
            {
                let y = 11;   # illegal shadowing, even in a new block
            }
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error2.contains("Redeclaration") || error2.contains("redeclaration")) &&
                (error2.contains("variable") || error2.contains("symbol")) && error2.contains("y"));

    const std::string code3 = R"(
        fn foo(x: int): {}

        fn main(): {
            let x = 1;
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE((error3.contains("Redeclaration") || error3.contains("redeclaration")) &&
                (error3.contains("variable") || error3.contains("symbol")) && error3.contains("x"));
}

TEST_F(NeutroniumTester, RedeclarationOfFunctionError) {
    const std::string code = R"(
        fn x(): {}
        fn x(y: bool): {}
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Redeclaration") &&
                (error.contains("symbol") || error.contains("function")) && error.contains("x"));
}

TEST_F(NeutroniumTester, UndeclaredFunctionError) {
    const std::string code = R"(
        fn main(): {
            x();
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("function") && error.contains("x"));
}

TEST_F(NeutroniumTester, FunctionCalledBeforeDeclarationError) {
    const std::string code = R"(
        fn main(): {
            x();
        }

        fn x(): {}
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && (error.contains("function") || error.contains("symbol")) && error.contains("x"));
}

TEST_F(NeutroniumTester, NestedFunctionsFails) {
    const std::string code = R"(
        fn outer(): {
            fn inner(): {}
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Invalid token") && error.contains("FN"));

    const std::string code2 = R"(
        fn outer(): {
            fn inner(): { exit 7; }
            inner();
        }
        outer();
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Invalid token") && error2.contains("FN"));
}

TEST_F(NeutroniumTester, AttemptToCallAVariableError) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            x();
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("call") && error.contains("function") && error.contains("x"));
}

TEST_F(NeutroniumTester, AttemptToAssignToAFunctionError) {
    const std::string code = R"(
        fn x(): {}

        fn main(): { x = 1; }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Assignment") && error.contains("variable") && error.contains("x"));

    const std::string code2 = R"(
        fn x(): {
            exit 0;
        }

        fn main(): {
            let y = x;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("variable") && error2.contains("not") && error2.contains("x"));
}

TEST_F(NeutroniumTester, FunctionArgumentsAreInvalid) {
    const std::string code = R"(
        fn x(a: int, b: bool): {
            exit 0;
        }

        fn main(): {
            x(1);          # missing arguments
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("function") || error.contains("Function")) &&
                error.contains("argument") && error.contains("x") && error.contains("2") &&
                error.contains("1"));

    const std::string code2 = R"(
        fn x(a: int, b: bool): {
            exit 0;
        }

        fn main(): {
            x(1, 2, 3);     # too many arguments
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error.contains("function") || error.contains("Function")) &&
                error2.contains("argument") && error2.contains("x") && error2.contains("2") &&
                error2.contains("3"));

    const std::string code3 = R"(
        fn x(a: int, mut b: bool): {
            exit 0;
        }

        fn main(): {
            x(1, 2);   # wrong type for argument 2
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE((error.contains("function") || error.contains("Function")) &&
                error3.contains("argument") && error3.contains("type") && error3.contains("x") &&
                error3.contains("b") && error3.contains("bool"));
}

TEST_F(NeutroniumTester, AssignmentToImmutableFunctionParamaterFails) {
    const std::string code = R"(
        fn x(mut a: int, b: bool): {
            a = 1;
            b = false;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("immutable") && error.contains("b") &&
                (error.contains("assignment") || error.contains("Assignment")));
}

TEST_F(NeutroniumTester, NonBooleanConditionError) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            if x: {
                exit 0;
            }
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("bool") && error.contains("condition") && error.contains("type") &&
                error.contains("integer"));

    const std::string code2 = R"(
        fn main(): {
            let x = 1;
            while x: {
                exit 0;
            }
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("bool") && error2.contains("condition") && error2.contains("type") &&
                error2.contains("integer"));
}

TEST_F(NeutroniumTester, InvalidIdentifierFails) {
    const std::string code = R"(
        fn main(): {
            let 42x = 5;  # invalid identifier
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("LITERAL") &&
                error.contains("IDENTIFIER"));
}

TEST_F(NeutroniumTester, UseAfterScopeFails) {
    const std::string code = R"(
        fn main(): {
            {
                let y = 2;
            }
            exit y;               # y is out of scope
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && (error.contains("variable") || error.contains("symbol")) && error.contains("y"));
}

TEST_F(NeutroniumTester, UnmatchedBracesFails) {
    const std::string code = R"(
        fn main(): {
            {let x = 1;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token"));
}

TEST_F(NeutroniumTester, UnmatchedParenthesesFails) {
    const std::string code = R"(
        fn main(): {
            let x = (1 + (2) * 3;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("RIGHT_PAREN"));
}

TEST_F(NeutroniumTester, UnaryOperatorOnWrongTypeFails) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            let y = !x;
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("operator") || error.contains("type")) &&
                error.contains("integer"));

    const std::string code2 = R"(
        fn main(): {
            let x = true;
            let y = -x;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error2.contains("operator") || error2.contains("type")) &&
                error2.contains("bool"));

    const std::string code3 = R"(
        fn x(): {}

        fn main(): { let y = -x(); }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE((error3.contains("operator") || error3.contains("type")) &&
                error3.contains("void"));
}

TEST_F(NeutroniumTester, BinaryOperatorOnWrongTypeFails) {
    const std::string code = R"(
        fn main(): {
            let x = true + false;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("type") && error.contains("integer") && error.contains("boolean"));

    const std::string code2 = R"(
        fn main(): {
            let x = true > false;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("type") && error2.contains("integer") &&
                error2.contains("boolean"));

    const std::string code3 = R"(
        fn x(): {}

        fn main(): {
            let y = (x() != x());
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("type") && error3.contains("void") && error3.contains("integer") &&
                error3.contains("boolean"));

    const std::string code4 = R"(
        fn x(): {}

        fn main(): {
            let y = (x() >= x());
        }
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("type") && error4.contains("void") && error4.contains("integer"));
}

TEST_F(NeutroniumTester, BreakWhenNotInLoopFails) {
    const std::string code = R"(
        fn main(): { break; }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("break") && error.contains("loop"));
}

TEST_F(NeutroniumTester, ContinueWhenNotInLoopFails) {
    const std::string code = R"(
        fn main(): { continue; }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("continue") && error.contains("loop"));
}