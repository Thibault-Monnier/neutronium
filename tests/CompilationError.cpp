#include "common/Tester.hpp"
#include "type/Trait.hpp"

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
    EXPECT_TRUE(error3.contains("Type mismatch") && error3.contains("int") &&
                error3.contains("void"));
}

TEST_F(NeutroniumTester, NonPlaceExpressionInLeftOfAssignmentFails) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            x + 2 = 3;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Left") && error.contains("place expression") &&
                error.contains("assignment"));

    const std::string code2 = R"(
        fn a() -> int: { return 0; }

        fn main(): {
            let x = 1;
            a() = 3;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Left") && error2.contains("place expression") &&
                error2.contains("assignment"));
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

    const std::string codeArrayModify = R"(
        fn main(): {
            let arr = [1, 2, 3];
            arr[0] = 10;    # illegal: arr is not mutable
        }
    )";
    auto [statusArrayModify, errorArrayModify] = compile(codeArrayModify);
    EXPECT_NE(statusArrayModify, 0);
    EXPECT_TRUE(
        errorArrayModify.contains("immutable") &&
        (errorArrayModify.contains("assignment") || errorArrayModify.contains("Assignment")) &&
        errorArrayModify.contains("arr"));

    const std::string codeArrayReassign = R"(
        fn main(): {
            let arr = [1, 2, 3];
            arr = [4, 5, 6];    # illegal: arr is not mutable
        }
    )";
    auto [statusArrayReassign, errorArrayReassign] = compile(codeArrayReassign);
    EXPECT_NE(statusArrayReassign, 0);
    EXPECT_TRUE(
        errorArrayReassign.contains("immutable") &&
        (errorArrayReassign.contains("assignment") || errorArrayReassign.contains("Assignment")) &&
        errorArrayReassign.contains("arr"));
}

TEST_F(NeutroniumTester, ReassignmentDifferentTypeFails) {
    const std::string codeIntSizes = R"(
        fn main(): {
            let mut x: int32 = 1;
            let y: int64 = 2;
            x = y;       # illegal: int32 -> int64
        }
    )";
    auto [statusIntSizes, errorIntSizes] = compile(codeIntSizes);
    EXPECT_NE(statusIntSizes, 0);
    EXPECT_TRUE(errorIntSizes.contains("Type mismatch") && errorIntSizes.contains("int32") &&
                errorIntSizes.contains("int64"));

    const std::string code = R"(
        fn main(): {
            let mut x: int = 1;
            x = true;      # illegal: int -> bool
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        fn main(): {
            let mut x: bool = true;
            x = 1;       # illegal: bool -> int
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
        fn main(): {}
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("Type mismatch") && error4.contains("bool") &&
                error4.contains("int"));

    const std::string codeArrayLength = R"(
        fn main(): {
            let mut arr = [[1,0], [2,5], [3,8]];
            arr = [4, 5, 4];    # illegal: array length mismatch
        }
    )";
    auto [statusArrayLength, errorArrayLength] = compile(codeArrayLength);
    EXPECT_NE(statusArrayLength, 0);
    EXPECT_TRUE(errorArrayLength.contains("Type mismatch") && errorArrayLength.contains("array"));

    const std::string codeArrayElementType = R"(
        fn main(): {
            let mut arr = [1, 2, 3];
            arr = [true, false, true];    # illegal: array element type mismatch
        }
    )";
    auto [statusArrayElementType, errorArrayElementType] = compile(codeArrayElementType);
    EXPECT_NE(statusArrayElementType, 0);
    EXPECT_TRUE(errorArrayElementType.contains("Type mismatch") &&
                errorArrayElementType.contains("array"));

    const std::string codeArrayElementType2 = R"(
        fn main(): {
            let mut arr = [1, 2, 3];
            arr[0] = true;    # illegal: array element type mismatch
        }
    )";
    auto [statusArrayElementType2, errorArrayElementType2] = compile(codeArrayElementType2);
    EXPECT_NE(statusArrayElementType2, 0);
    EXPECT_TRUE(errorArrayElementType2.contains("Type mismatch") &&
                errorArrayElementType2.contains("int") && errorArrayElementType2.contains("bool"));

    const std::string codeArrayElementType3 = R"(
        fn main(): {
            let mut arr: [[int; 2]; 2] = [[1,2], [3,4]];
            let x: int8 = 5;
            arr[0][0] = x;    # illegal: int -> int8
        }
    )";
    auto [statusArrayElementType3, errorArrayElementType3] = compile(codeArrayElementType3);
    EXPECT_NE(statusArrayElementType3, 0);
    EXPECT_TRUE(errorArrayElementType3.contains("Type mismatch") &&
                errorArrayElementType3.contains("int8") && errorArrayElementType3.contains("int"));
}

TEST_F(NeutroniumTester, ReassignmentDifferentInferredTypeFails) {
    const std::string code = R"(
        fn main(): {
            let mut flag = true;
            flag = 1;       # illegal: bool -> int
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch"));

    const std::string code2 = R"(
        fn main(): {
            let mut x = 1;
            x = true;      # illegal: int -> bool
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

    const std::string code4 = R"(
        fn main(): {
            let mut a = 1;
            let b: int8 = 2;
            a = b;              # a inferred as int8
            let x: int16 = 3;
            let y = x;          # x inferred as int16

            x * a;              # illegal: int16 * int8
        }
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("Type mismatch") && error4.contains("int8") &&
                error4.contains("int"));
}

TEST_F(NeutroniumTester, ConflictingIntegerInferenceFails) {
    const std::string codeFunctionParam = R"(
        fn use8(a: int8): {}
        fn use16(a: int16): {}

        fn main(): {
            let n = 1;
            use8(n);     # n inferred as int8
            use16(n);    # conflicting: n also required to be int16
        }
    )";
    auto [statusFunctionParam, errorFunctionParam] = compile(codeFunctionParam);
    EXPECT_NE(statusFunctionParam, 0);
    EXPECT_TRUE(errorFunctionParam.contains("Type mismatch") &&
                errorFunctionParam.contains("int8") && errorFunctionParam.contains("int16"));

    const std::string codeBinaryOp = R"(
        fn main(): {
            let a: int8 = 1;
            let b: int16 = 2;
            let c = a + b;   # illegal: int8 + int16
        }
    )";
    auto [statusBinaryOp, errorBinaryOp] = compile(codeBinaryOp);
    EXPECT_NE(statusBinaryOp, 0);
    EXPECT_TRUE(errorBinaryOp.contains("Type mismatch") && errorBinaryOp.contains("int8") &&
                errorBinaryOp.contains("int16"));
}

TEST_F(NeutroniumTester, CompoundAssignmentWithNonIntegersFails) {
    const std::string code2 = R"(
        fn main(): {
            let mut x = true;
            x *= false;       # illegal: bool *= bool
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type") && error2.contains("trait") &&
                error2.contains(traitToString(Trait::MUL)) && error2.contains("bool"));

    const std::string code3 = R"(
        fn main(): {
            let mut x = [[false], [false]];
            let y = [[true], [false]];
            x[0] -= y[1];
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type") && error3.contains("trait") &&
                error3.contains(traitToString(Trait::SUB)) && error3.contains("bool") &&
                error3.contains("array"));
}

TEST_F(NeutroniumTester, WrongSpecifiedTypeFails) {
    const std::string code = R"(
        fn main(): {
            let x: int = true != false;  # illegal: bool -> int
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("bool") && error.contains("int"));

    const std::string code2 = R"(
        fn main(): {
            let mut x: bool = 1 * -5;   # illegal: int -> bool
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch") && error2.contains("int") &&
                error2.contains("bool"));

    const std::string codeArrays = R"(
        fn main(): {
            let arr: [bool; 2] = [1, 2];
        }
    )";
    auto [statusArrays, errorArrays] = compile(codeArrays);
    EXPECT_NE(statusArrays, 0);
    EXPECT_TRUE(errorArrays.contains("Type mismatch") && errorArrays.contains("bool") &&
                errorArrays.contains("int"));
}

TEST_F(NeutroniumTester, DifferentElementTypesInArrayFails) {
    const std::string code = R"(
        fn main(): {
            let arr = [1, true, 3];  # illegal: mixed Type in array
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("int") && error.contains("bool"));

    const std::string codeInnerElement = R"(
        fn main(): {
            let arr = [[1, 2], [true, false]];  # illegal: mixed Type in inner arrays
        }
    )";
    auto [statusInnerElement, errorInnerElement] = compile(codeInnerElement);
    EXPECT_NE(statusInnerElement, 0);
    EXPECT_TRUE(errorInnerElement.contains("Type mismatch") &&
                errorInnerElement.contains("array") && errorInnerElement.contains("int") &&
                errorInnerElement.contains("bool"));

    const std::string codeInnerElementSize = R"(
        fn main(): {
            let arr = [[5, 6], [7, 8, 9]];  # illegal: inner arrays have different sizes
        }
    )";
    auto [statusInnerElementSize, errorInnerElementSize] = compile(codeInnerElementSize);
    EXPECT_NE(statusInnerElementSize, 0);
    EXPECT_TRUE(errorInnerElementSize.contains("Type mismatch") &&
                errorInnerElementSize.contains("array") && errorInnerElementSize.contains("2") &&
                errorInnerElementSize.contains("3"));
}

TEST_F(NeutroniumTester, EmptyArrayFails) {
    const std::string code = R"(
        fn main(): {
            let arr: [bool; 0] = [];  # illegal: empty array
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("empty") && error.contains("Array literal"));
}

TEST_F(NeutroniumTester, ExitWithBooleanExpressionFails) {
    const std::string code = R"(
        fn main(): {
            let ok = true;
            exit ok;         # exit expects int expression
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("int") && error.contains("bool"));
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

TEST_F(NeutroniumTester, VoidVariableType) {
    const std::string code = R"(
        fn foo(): {}

        fn main(): {
            let x = foo();
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type") && error.contains("void") && error.contains("storable"));
}

TEST_F(NeutroniumTester, AttemptToSubscriptANonArray) {
    const std::string code = R"(
        fn main(): {
            let x = 1;
            x[0];
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type") && error.contains("trait") &&
                error.contains(traitToString(Trait::SUBSCRIPT)) && error.contains("int"));

    const std::string code2 = R"(
        fn a(): {}

        fn main(): {
            a[0];
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("not") && error2.contains("variable") && error2.contains("`a`"));

    const std::string code3dArray = R"(
        fn getArray(a: int8, b: int8, c: int8, d: int8) -> [[int8; 2]; 2]: {
            return [[a, b], [c, d]];
        }

        fn main(): {
            let arr = getArray(1, 2, 3, 4);
            let int8Bits: int8 = 2;
            let val = arr[1][1] / int8Bits;
            arr[0][1][2];      # arr is 2D, but accessed as 3D
        }
    )";
    auto [status3dArray, error3dArray] = compile(code3dArray);
    EXPECT_NE(status3dArray, 0);
    EXPECT_TRUE(error3dArray.contains("Type") && error3dArray.contains("trait") &&
                error3dArray.contains(traitToString(Trait::SUBSCRIPT)) &&
                error3dArray.contains("int8"));
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
    EXPECT_TRUE(error3.contains("Type mismatch") && error3.contains("int") &&
                error3.contains("bool"));

    const std::string codeArray = R"(
        fn x(arr: [int; 3]): {
            exit 0;
        }

        fn main(): {
            x([1, 2]);  # wrong array length
        }
    )";
    auto [statusArray, errorArray] = compile(codeArray);
    EXPECT_NE(statusArray, 0);
    EXPECT_TRUE(errorArray.contains("Type mismatch") && errorArray.contains("array") &&
                errorArray.contains("2") && errorArray.contains("3"));
}

TEST_F(NeutroniumTester, FunctionReturnTypeMismatch) {
    const std::string code = R"(
        fn x() -> bool: {
            return 1;
        }
        fn main(): {}
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("bool") && error.contains("int"));

    const std::string code2 = R"(
        fn x(): {
            return true;
        }
        fn main(): {}
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type mismatch") && error2.contains("void") &&
                error2.contains("bool"));

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
    EXPECT_TRUE(error3.contains("Type mismatch") && error3.contains("bool") &&
                error3.contains("int"));
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

TEST_F(NeutroniumTester, AssignmentToImmutableFunctionParameterFails) {
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

TEST_F(NeutroniumTester, FunctionParametersWithoutCommasFails) {
    const std::string code = R"(
        fn x(a: int b: bool): {   # missing comma
            exit 0;
        }

        fn main(): {}
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("token") && error.contains("expected") &&
                error.contains("IDENTIFIER"));
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
    EXPECT_TRUE(error.contains("Type mismatch") && error.contains("bool") && error.contains("int"));

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
    EXPECT_TRUE(error2.contains("Type mismatch") && error2.contains("bool") &&
                error2.contains("int"));
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
    EXPECT_TRUE(error.contains("Type") && error.contains("trait") &&
                error.contains(traitToString(Trait::NOT)) && error.contains("int"));

    const std::string code2 = R"(
        fn main(): {
            let x = true;
            let y = -x;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type") && error2.contains("trait") &&
                error2.contains(traitToString(Trait::SUB)) && error2.contains("bool"));

    const std::string code3 = R"(
        fn x(): {}

        fn main(): { let y = -x(); }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type") && error3.contains("trait") &&
                error3.contains(traitToString(Trait::SUB)) && error3.contains("void"));
}

TEST_F(NeutroniumTester, BinaryOperatorOnWrongTypeFails) {
    const std::string code = R"(
        fn main(): {
            let x = true + false;
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);
    EXPECT_TRUE(error.contains("Type") && error.contains("trait") &&
                error.contains(traitToString(Trait::ADD)) && error.contains("bool"));

    const std::string code2 = R"(
        fn main(): {
            let x = true > false;
        }
    )";
    auto [status2, error2] = compile(code2);
    EXPECT_NE(status2, 0);
    EXPECT_TRUE(error2.contains("Type") && error2.contains("trait") &&
                error2.contains(traitToString(Trait::GT)) && error2.contains("bool"));

    const std::string code3 = R"(
        fn x(): {}

        fn main(): {
            let y = (x() != x());
        }
    )";
    auto [status3, error3] = compile(code3);
    EXPECT_NE(status3, 0);
    EXPECT_TRUE(error3.contains("Type") && error3.contains("trait") &&
                error3.contains(traitToString(Trait::EQ)) && error3.contains("void"));

    const std::string code4 = R"(
        fn x(): {}

        fn main(): {
            let y = (x() >= x());
        }
    )";
    auto [status4, error4] = compile(code4);
    EXPECT_NE(status4, 0);
    EXPECT_TRUE(error4.contains("Type") && error4.contains("trait") &&
                error4.contains(traitToString(Trait::GTE)) && error4.contains("void"));
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

TEST_F(NeutroniumTester, MultipleErrorsAllReported) {
    const std::string code = R"(
        extern fn mod(a: int, b: int) -> int;

        fn foo(a: int, b: int, c: bool) -> int8: {}

        fn mains(): {
            foo(1, true);
            let mut arr = [];
            arr[0] = 1;
            foo+1 = 3;
            let x = foso();
            x = 1;
            x = 3;
            {
                let mut y = x + 2;
            }
            y += false - truer;
            exit -mod(-x+true, 256*false);
        }
    )";
    auto [status, error] = compile(code);
    EXPECT_NE(status, 0);

    EXPECT_TRUE(error.contains("Function") && error.contains("foo") && error.contains("return"));
    EXPECT_TRUE(error.contains("foo") && error.contains("arguments") &&
                error.contains("expected 3"));
    EXPECT_TRUE(error.contains("Array") && error.contains("literal") && error.contains("empty"));
    EXPECT_TRUE(error.contains("Left-hand") && error.contains("assignment") &&
                error.contains("expression"));
    EXPECT_TRUE(error.contains("undeclared") && error.contains("function") &&
                error.contains("foso"));
    EXPECT_TRUE(error.contains("Assignment") && error.contains("immutable") && error.contains("x"));
    EXPECT_TRUE(error.contains("Assignment") && error.contains("undeclared") &&
                error.contains("variable"));
    EXPECT_TRUE(error.contains("undeclared") && error.contains("symbol") &&
                error.contains("truer"));
    EXPECT_TRUE(error.contains("main") && error.contains("function") &&
                error.contains("executable target"));
}