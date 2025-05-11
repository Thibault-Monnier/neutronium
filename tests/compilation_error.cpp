#include "common/tester.hpp"

TEST_F(NeutroniumTester, ImmutableReassignmentFails) {
    const std::string code = R"(
        let x = 1;
        x = 2;          # illegal: x is immutable
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
}

TEST_F(NeutroniumTester, MutableReassignmentDifferentTypeFails) {
    const std::string code = R"(
        let mut flag = true;
        flag = 1;       # illegal: boolean -> integer
        exit 0;
    )";
    EXPECT_NE(compile(code).first, 0);
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

TEST_F(NeutroniumTester, TypeMismatchInBinaryOpFails) {
    const std::string code = R"(
        let a = 1;
        let b = true;
        exit a + b;      # int + bool is illegal
    )";
    EXPECT_NE(compile(code).first, 0);
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
    EXPECT_TRUE(error.find("break") != std::string::npos &&
                error.find("outside of a loop") != std::string::npos);
}

TEST_F(NeutroniumTester, ContinueWhenNotInLoopFails) {
    const std::string code = R"(
        {
            continue;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.find("continue") != std::string::npos &&
                error.find("outside of a loop") != std::string::npos);
}