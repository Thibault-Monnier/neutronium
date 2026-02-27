# Neutronium Roadmap (Syntax & Features)

This document presents planned features and language constructs for Neutronium. The roadmap is divided into
two main phases: _basic_ and _advanced_. The basic phase contains simple constructs to make the language easier and
friendlier to use, while the advanced phase introduces more complex features that might be implemented in the distant
future.

All of the following is subject to change.

## Basic

### 1) Literals

- [ ] Floating-point numbers: `float32` and `float64`
- [ ] Characters and strings: `'a'`, `"hello world"`
- [ ] Unsigned integers: `uint8/16/32/64`
- [ ] Numeric literals improvements:
    - [X] `_` digit separator (`1_000_000`)
    - [ ] hex `0x`
    - [ ] bin `0b`
    - [ ] oct `0o`

### 2) Arrays

- [X] Sized array literal shorthand: `[expr; N]` for an array containing `N` copies of `expr`
- [ ] Length inference: `let a: [int] = [1,2,3];` instead of `let a: [int; 3] = [1,2,3];`

### 3) Operators

- [X] Logical operators: `&&` and `||`
- [ ] Increment/decrement: `++` and `--` (postfix) as syntactic sugar for `+= 1` and `-= 1`. They are statements, not
  expressions.
- [ ] Bitwise operators: `&`, `|`, `^`, `~`, `<<`, `>>`

### 4) Control flow

- [ ] `for` loops:
    - [ ] Range-based: `for i in 0..n:`
    - [ ] Iterator-based: `for x in collection:`

### 5) Functions & declarations

- [ ] Declaration without initialization: `let x: [int; 500];`
- [ ] Function pointer types: enables passing functions as arguments and returning them from functions `fn(int)->int`
- [ ] `const` for compile-time constants: `const PI: float64 = 3.14159;`. Only in global scope.

---

## Advanced

### 1) Modules

- [ ] `import` statements:
    - [ ] `import Foo;`: all symbols from module Foo
    - [ ] `import Foo::bar;`: specific symbol
    - [ ] `import Foo::{bar, baz};`: multiple symbols
- [ ] All files are modules; maybe support specific module declaration

### 2) Data structures

- [ ] Structs: only contains data fields, no methods` struct Point { x:int; y:int; };`
- [ ] Classes: data fields + methods + constructors + inheritance
- [ ] Enums with variants: `enum Result { Ok(int), Err(string) };`
- [ ] Pattern matching with match or switch: `match/switch value: { case Pattern1: ...; case Pattern2: ...; }`

### 3) Functional programming

- [ ] Lambdas/anonymous functions: `let add = fn(a:int, b:int) -> int: { a + b };`. Revise this syntax.

### 4) Generics

- [ ] Generics: `fn foo<T>(bar: T) -> T: {}`
- [ ] Trait definitions and implementations

### 5) Memory & runtime

- [ ] Garbage collector or ARC for automatic memory management
- [ ] Pointer/reference types but not pointer arithmetic
- [ ] Concurrency support: `async` and `await` keywords
