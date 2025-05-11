#include "common/tester.hpp"

TEST_F(NeutroniumTester, ImmutableReassignmentFails) {
    const std::string code = R"(
        let x = 1;
        x = 2;          # illegal: x is immutable
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, ReassignmentDifferentTypeFails) {
    const std::string code = R"(
        let mut x: int = 1;
        x = true;      # illegal: integer -> boolean
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        let mut x: bool = true;
        x = 1;       # illegal: boolean -> integer
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));

    const std::string code3 = R"(
        let mut x: int = 1;
        let y: bool = true;
        x = y;       # illegal: int -> bool
        exit 0;
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type mismatch"));
}

TEST_F(NeutroniumTester, ReassignmentDifferentInferredTypeFails) {
    const std::string code = R"(
        let mut flag = true;
        flag = 1;       # illegal: boolean -> integer
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        let mut x = 1;
        x = true;      # illegal: integer -> boolean
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));

    const std::string code3 = R"(
        let mut x = false;
        let y = 1;
        x = y;       # illegal: bool -> int
        exit 0;
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type mismatch"));
}

TEST_F(NeutroniumTester, WrongSpecifiedTypeFails) {
    const std::string code = R"(
        let x: int = true != false;  # illegal: bool -> int
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("bool") && error.contains("int") && error.contains("type"));

    const std::string code2 = R"(
        let mut x: bool = 1 * -5;   # illegal: int -> bool
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("int") && error2.contains("bool") && error2.contains("type"));
}

TEST_F(NeutroniumTester, VariableShadowingAcrossBlocksFails) {
    const std::string code = R"(
        {
            let y = 10;
        }
        {
            let y = 11;   # illegal shadowing, even in a new block
        }
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, ExitWithBooleanExpressionFails) {
    const std::string code = R"(
        let ok = true;
        exit ok;         # exit expects integer expression
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, TypeMismatchInExpressionFails) {
    const std::string code = R"(
        let a = 1;
        let b = true;
        let c = a + b;      # int + bool is illegal
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        let a = 1;
        let b = true;
        let c = a == b;     # int == bool is illegal
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch"));
}

TEST_F(NeutroniumTester, VariableShadowingError) {
    const std::string code = R"(
        let x = 1;
        {
            let x = 2;
        }

        exit 0;
    )";

    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UndeclaredVariableError) {
    const std::string code = R"(
        let x = y;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UndeclaredVariableError2) {
    const std::string code = R"(
        x = 1;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, VariableUsedBeforeDeclarationError) {
    const std::string code = R"(
        let y = x + 1;
        let x = 1;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UndeclaredFunctionError) {
    const std::string code = R"(
        let x = 1;
        y();
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, FunctionCalledBeforeDeclarationError) {
    const std::string code = R"(
        y();
        fn y: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, AttemptToCallAVariableError) {
    const std::string code = R"(
        let x = 1;
        x();
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, AttemptToUseAFunctionAsAVariableError) {
    const std::string code = R"(
        fn x: {
            exit 0;
        }
        let y = x;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, AttemptToAssignToAFunctionError) {
    const std::string code = R"(
        fn x: {
            exit 0;
        }
        x = 1;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, RedeclarationOfFunctionError) {
    const std::string code = R"(
        fn x: {
            exit 0;
        }
        fn x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, RedeclarationOfVariableError) {
    const std::string code = R"(
        let x = 1;
        let x = 2;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, NonBooleanConditionInIfError) {
    const std::string code = R"(
        let x = 1;
        if x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, NonBooleanConditionInWhileError) {
    const std::string code = R"(
        let x = 1;
        while x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, InvalidIdentifierFails) {
    const std::string code = R"(
        let 42x = 5;  # invalid identifier
        exit 0;
    )";

    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UseAfterScopeFails) {
    const std::string code = R"(
        {
            let y = 2;
        }
        exit y;               # y is out of scope
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, VarFunctionNameClashFails) {
    const std::string code = R"(
        fn bar: { }
        let bar = 1;          # clashes with fn bar
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, DoubleLogicalNotRejected) {
    const std::string code = R"(
        if !!true: {          # operand of '!' must be a primary expression
            exit 1;
        } else: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, MissingSemicolonFails) {
    const std::string code = R"( exit 1 )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, EmptyStatementsDisallowed) {
    const std::string code = R"(
        ;;
        let x = 1;;
        exit x;;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, DeclarationWithoutInitializerFails) {
    const std::string code = R"(
        let x;
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UnmatchedBracesFails) {
    const std::string code = R"(
        {let x = 1;
        if x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, UnmatchedParenthesesFails) {
    const std::string code = R"(
        let x = (1 + (2) * 3;
        if x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, InvalidUnaryOperatorFails) {
    const std::string code = R"(
        let x = 1;
        let y = !x;
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, InvalidUnaryOperatorFails2) {
    const std::string code = R"(
        let x = true;
        let y = -x;
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, InvalidTypeForUnaryOperatorFails) {
    const std::string code = R"(
        fn x: {}
        let y = -x();
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, ArithmeticOperatorOnBooleanFails) {
    const std::string code = R"(
        let x = true + false;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, EqualityOperatorOnEmptyFails) {
    const std::string code = R"(
        fn x: {}
        let y = (x() != x());
    )";

    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, RelationalOperatorOnBooleanFails) {
    const std::string code = R"(
        let x = true > false;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, RelationalOperatorOnEmptyFails) {
    const std::string code = R"(
        fn x: {}
        let y = (x() >= x());
    )";

    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, EmptyVariableAssignmentFails) {
    const std::string code = R"(
        fn x: {}
        let y = x();
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, BreakWhenNotInLoopFails) {
    const std::string code = R"(
        break;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("break") && error.contains("outside of a loop"));
}

TEST_F(NeutroniumTester, ContinueWhenNotInLoopFails) {
    const std::string code = R"(
        {
            continue;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("continue") && error.contains("outside of a loop"));
}