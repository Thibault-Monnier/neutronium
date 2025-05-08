#include "common/tester.hpp"

TEST_F(NeutroniumTester, ImmutableReassignmentFails) {
    const std::string code = R"(
        let x = 1;
        x = 2;          # illegal: x is immutable
        exit 0;
    )";
    std::cout << compile(code) << "   <----------- OUTPUT\n";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, MutableReassignmentDifferentTypeFails) {
    const std::string code = R"(
        let mut flag = true;
        flag = 1;       # illegal: boolean -> integer
        exit 0;
    )";
    EXPECT_NE(compile(code), 0);
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
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, ExitWithBooleanExpressionFails) {
    const std::string code = R"(
        let ok = true;
        exit ok;         # exit expects integer expression
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, TypeMismatchInBinaryOpFails) {
    const std::string code = R"(
        let a = 1;
        let b = true;
        exit a + b;      # int + bool is illegal
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, VariableShadowingError) {
    const std::string code = R"(
        let x = 1;
        {
            let x = 2;
        }

        exit 0;
    )";

    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, UndeclaredVariableError) {
    const std::string code = R"(
        let x = y;
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, UndeclaredVariableError2) {
    const std::string code = R"(
        x = 1;
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, VariableUsedBeforeDeclarationError) {
    const std::string code = R"(
        let y = x + 1;
        let x = 1;
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, UndeclaredFunctionError) {
    const std::string code = R"(
        let x = 1;
        y();
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, FunctionUsedBeforeDeclarationError) {
    const std::string code = R"(
        y();
        fn y: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, AttemptToCallAVariableError) {
    const std::string code = R"(
        let x = 1;
        x();
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, AttemptToUseAFunctionAsAVariableError) {
    const std::string code = R"(
        fn x: {
            exit 0;
        }
        let y = x;
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, AttemptToAssignToAFunctionError) {
    const std::string code = R"(
        fn x: {
            exit 0;
        }
        x = 1;
    )";
    EXPECT_NE(compile(code), 0);
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
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, RedeclarationOfVariableError) {
    const std::string code = R"(
        let x = 1;
        let x = 2;
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, NonBooleanConditionInIfError) {
    const std::string code = R"(
        let x = 1;
        if x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code), 0);
}

TEST_F(NeutroniumTester, NonBooleanConditionInWhileError) {
    const std::string code = R"(
        let x = 1;
        while x: {
            exit 0;
        }
    )";
    EXPECT_NE(compile(code), 0);
}

