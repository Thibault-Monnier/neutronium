#include "common/tester.hpp"

TEST_F(NeutroniumTester, ImmutableReassignmentFails) {
    const std::string code = R"(
        let x = 1;
        x = 2;          # illegal: x is immutable
        exit 0;
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

    const std::string code4 = R"(
        fn foo(mut x: bool): {
            x = 1;      # illegal: bool -> int
        }
        exit 0;
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("Type mismatch") && error4.contains("x"));
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
    EXPECT_TRUE(error.contains("bool") && error.contains("int") && error.contains("type") &&
                error.contains("x"));

    const std::string code2 = R"(
        let mut x: bool = 1 * -5;   # illegal: int -> bool
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("int") && error2.contains("bool") && error2.contains("type") &&
                error2.contains("x"));
}

TEST_F(NeutroniumTester, ExitWithBooleanExpressionFails) {
    const std::string code = R"(
        let ok = true;
        exit ok;         # exit expects integer expression
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("exit") && error.contains("integer") && error.contains("type") &&
                error.contains("boolean"));
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

TEST_F(NeutroniumTester, RedeclarationOfVariableError) {
    const std::string code = R"(
        let x = 1;
        let x = 2;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("Redeclaration") || error.contains("redeclaration")) &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("x"));
}

TEST_F(NeutroniumTester, UndeclaredVariableError) {
    const std::string code = R"(
        let x = y;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("variable") && error.contains("y"));

    const std::string code2 = R"(
        x = 1;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("undeclared") && error2.contains("variable") &&
                error2.contains("x"));
}

TEST_F(NeutroniumTester, VariableUsedBeforeDeclarationError) {
    const std::string code = R"(
        let y = x + 1;
        let x = 1;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("variable") && error.contains("x"));
}

TEST_F(NeutroniumTester, VariableOfTypeVoidFails) {
    const std::string code = R"(
        fn x(): {}
        let y = x();
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("void") && error.contains("type") && error.contains("y"));
}

TEST_F(NeutroniumTester, SymbolShadowingError) {
    const std::string code = R"(
        let x = 1;
        {
            let x = 2;
        }

        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("Redeclaration") || error.contains("redeclaration")) &&
                (error.contains("variable") || error.contains("symbol")) && error.contains("x"));

    const std::string code2 = R"(
        {
            let y = 10;
        }
        {
            let y = 11;   # illegal shadowing, even in a new block
        }
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error2.contains("Redeclaration") || error2.contains("redeclaration")) &&
                (error2.contains("variable") || error2.contains("symbol")) && error2.contains("y"));

    /*const std::string code3 = R"(
        fn x(): {
            let x = 1;
        }
        {
            fn x(): {
                let x = 2;   # illegal shadowing, even in a new block
            }
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE((error3.contains("Redeclaration") || error3.contains("redeclaration")) &&
                (error3.contains("function") || error3.contains("symbol")) &&
    error3.contains("x"));*/

    const std::string code4 = R"(
        let x = 1;
        fn foo(x: int): {}
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE((error4.contains("Redeclaration") || error4.contains("redeclaration")) &&
                (error4.contains("variable") || error4.contains("symbol")) && error4.contains("x"));

    const std::string code5 = R"(
        fn foo(x: int): {
            let x = 1;
        }
    )";
    auto [status5, error5] = compile(code5);
    EXPECT_NE(status5, 0);
    EXPECT_TRUE((error5.contains("Redeclaration") || error5.contains("redeclaration")) &&
                (error5.contains("function") || error5.contains("symbol")) && error5.contains("x"));
}

TEST_F(NeutroniumTester, RedeclarationOfFunctionError) {
    const std::string code = R"(
        fn x(): {
            exit 0;
        }
        fn x(y: bool): {
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Redeclaration") &&
                (error.contains("symbol") || error.contains("function")) && error.contains("x"));
}

TEST_F(NeutroniumTester, UndeclaredFunctionError) {
    const std::string code = R"(
        let x = 1;
        y();
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("function") && error.contains("y"));
}

TEST_F(NeutroniumTester, FunctionCalledBeforeDeclarationError) {
    const std::string code = R"(
        y();
        fn y(): {
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("function") && error.contains("y"));
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
        let x = 1;
        x();
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("call") && error.contains("function") && error.contains("x"));
}

TEST_F(NeutroniumTester, AttemptToAssignToAFunctionError) {
    const std::string code = R"(
        fn x(): {
            exit 0;
        }
        x = 1;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Assignment") && error.contains("variable") && error.contains("x"));

    const std::string code2 = R"(
        fn x(): {
            exit 0;
        }
        let y = x;
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
        x(1);          # missing arguments
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
        x(1, 2, 3);     # too many arguments
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
        x(1, 2);   # wrong type for argument 2
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

TEST_F(NeutroniumTester, NonBooleanConditionInIfError) {
    const std::string code = R"(
        let x = 1;
        if x: {
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("bool") && error.contains("condition") && error.contains("type") &&
                error.contains("integer"));
}

TEST_F(NeutroniumTester, NonBooleanConditionInWhileError) {
    const std::string code = R"(
        let x = 1;
        while x: {
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("bool") && error.contains("condition") && error.contains("type") &&
                error.contains("integer"));
}

TEST_F(NeutroniumTester, InvalidIdentifierFails) {
    const std::string code = R"(
        let 42x = 5;  # invalid identifier
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("LITERAL") &&
                error.contains("IDENTIFIER"));
}

TEST_F(NeutroniumTester, UseAfterScopeFails) {
    const std::string code = R"(
        {
            let y = 2;
        }
        exit y;               # y is out of scope
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("undeclared") && error.contains("variable") && error.contains("y"));
}

TEST_F(NeutroniumTester, VarFunctionNameClashFails) {
    const std::string code = R"(
        fn bar(): {}
        let bar = 1;          # clashes with fn bar
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Redeclaration") && error.contains("symbol") &&
                error.contains("bar"));
}

TEST_F(NeutroniumTester, DoubleLogicalNotRejected) {
    const std::string code = R"(
        if !!true: {          # operand of '!' must be a primary expression
            exit 1;
        } else: {
            exit 0;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("primary") && error.contains("expression") &&
                error.contains("BANG") && error.contains("token"));
}

TEST_F(NeutroniumTester, MissingSemicolonFails) {
    const std::string code = R"( exit 1 )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("EOF") && error.contains("SEMICOLON"));
}

TEST_F(NeutroniumTester, EmptyStatementsNotAllowed) {
    const std::string code = R"(
        ;;
        let x = 1;;
        exit x;;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("SEMICOLON"));
}

TEST_F(NeutroniumTester, DeclarationWithoutInitializerFails) {
    const std::string code = R"(
        let x;
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("EQUAL") && error.contains("SEMICOLON") &&
                error.contains("x"));
}

TEST_F(NeutroniumTester, UnmatchedBracesFails) {
    const std::string code = R"(
        {let x = 1;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token"));
}

TEST_F(NeutroniumTester, UnmatchedParenthesesFails) {
    const std::string code = R"(
        let x = (1 + (2) * 3;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("RIGHT_PAREN"));
}

TEST_F(NeutroniumTester, UnaryOperatorOnWrongTypeFails) {
    const std::string code = R"(
        let x = 1;
        let y = !x;
        exit 0;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE((error.contains("operator") || error.contains("type")) &&
                error.contains("integer"));

    const std::string code2 = R"(
        let x = true;
        let y = -x;
        exit 0;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE((error2.contains("operator") || error2.contains("type")) &&
                error2.contains("bool"));

    const std::string code3 = R"(
        fn x(): {}
        let y = -x();
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE((error3.contains("operator") || error3.contains("type")) &&
                error3.contains("void"));
}

TEST_F(NeutroniumTester, BinaryOperatorOnWrongTypeFails) {
    const std::string code = R"(
        let x = true + false;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("type") && error.contains("integer") && error.contains("boolean"));

    const std::string code2 = R"(
        let x = true > false;
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("type") && error2.contains("integer") &&
                error2.contains("boolean"));

    const std::string code3 = R"(
        fn x(): {}
        let y = (x() != x());
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("type") && error3.contains("void") && error3.contains("integer") &&
                error3.contains("boolean"));

    const std::string code4 = R"(
        fn x(): {}
        let y = (x() >= x());
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("type") && error4.contains("void") && error4.contains("integer"));
}

TEST_F(NeutroniumTester, BreakWhenNotInLoopFails) {
    const std::string code = R"(
        break;
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("break") && error.contains("loop"));
}

TEST_F(NeutroniumTester, ContinueWhenNotInLoopFails) {
    const std::string code = R"(
        {
            continue;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("continue") && error.contains("loop"));
}