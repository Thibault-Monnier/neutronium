#include "common/tester.hpp"

TEST_F(NeutroniumTester, StatementOusideOfFunctionFails) {
    const std::string code = R"(
        let x = 1;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("LET") && error.contains("expected"));

    const std::string code2 = R"(
        1 + 2;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("token") && error2.contains("LITERAL") &&
                error2.contains("expected"));

    const std::string code3 = R"(
        fn x(): {}
        x();
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("token") && error3.contains("IDENTIFIER") &&
                error3.contains("expected"));

    const std::string code4 = R"(
        {}
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("token") && error4.contains("LEFT_BRACE") &&
                error4.contains("expected"));
}

TEST_F(NeutroniumTester, FunctionDeclarationNotInGlobalScopeFails) {
    const std::string code = R"(
        fn main(): {
            fn x(): {}
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("FN"));
}

TEST_F(NeutroniumTester, MainFunctionErrors) {
    const std::string code = R"(
        fn x(): {}
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("No") && error.contains("main") && error.contains("function"));

    const std::string code2 = R"(
        fn main(mut x: int): {}
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("main") && error2.contains("function") &&
                error2.contains("parameter"));

    const std::string code3 = R"(
        fn main() -> int: {
            return 1;
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("main") && error3.contains("function") &&
                error3.contains("return") && error3.contains("void"));
}

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
    EXPECT_TRUE(error4.contains("Type mismatch"));
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
    EXPECT_TRUE(error.contains("undeclared") &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("y"));

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
    EXPECT_TRUE(error.contains("undeclared") &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("x"));
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
        fn foo(mut x: int): {
            let x = 2;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error2.contains("Redeclaration") || error2.contains("redeclaration")) &&
                (error2.contains("variable") || error2.contains("symbol")) && error2.contains("x"));
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
    EXPECT_TRUE(error.contains("undeclared") &&
                (error.contains("function") || error.contains("symbol")) && error.contains("x"));
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

TEST_F(NeutroniumTester, FunctionReturnTypeMismatch) {
    const std::string code = R"(
        fn x() -> bool: {
            return 1;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("return") &&
                error.contains("type") && error.contains("bool") && error.contains("int"));

    const std::string code2 = R"(
        fn x(): {
            return true;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch") && error2.contains("return") &&
                error2.contains("type") && error2.contains("void") && error2.contains("bool"));

    const std::string code3 = R"(
        fn x() -> int: {
            return 1;
        }

        fn main(): {
            let y: bool = x();
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type mismatch") && error3.contains("y") &&
                error3.contains("type") && error3.contains("bool") && error3.contains("int"));
}

TEST_F(NeutroniumTester, FunctionDoesNotAlwaysReturn) {
    const std::string code = R"(
        fn x() -> bool: {
            let a = 1;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Function") && error.contains("x") && error.contains("return") &&
                error.contains("always") && error.contains("bool"));

    const std::string code2 = R"(
        fn x() -> int: {
            if true: {
                return 2;
            } elif false: {
                return 3;
            } else: {
                let a = 1;  # no return here
            }

            while false: {
                return 4;
            }
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Function") && error2.contains("x") && error2.contains("return") &&
                error2.contains("always") && error2.contains("int"));
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

TEST_F(NeutroniumTester, FunctionCallWithoutCommasFails) {
    const std::string code = R"(
        fn x(a: int, b: bool): {
            exit 0;
        }

        fn main(): {
            x(1 true);  # missing comma
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("expected") && error.contains("TRUE"));
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
    EXPECT_TRUE(error2.contains("bool") && error2.contains("condition") &&
                error2.contains("type") && error2.contains("integer"));
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
    EXPECT_TRUE(error.contains("undeclared") &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("y"));
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

TEST_F(NeutroniumTester, InvalidTypeSpecifier) {
    const std::string code = R"(
        fn main(): {
            let x: int = 1;
            let y: invalidType = 2;  # invalid type
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("type specifier") &&
                error.contains("IDENTIFIER"));

    const std::string code2 = R"(
        fn main(): {
            let x: int = 1;
            let y: 1 = 2;  # invalid type
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("token") && error2.contains("type specifier") &&
                error2.contains("LITERAL"));
}