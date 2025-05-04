# Neutronium Language

Neutronium is a lightweight C-like programming language that supports basic constructs such as variables, functions, and
control flow.

## Features & Syntax

Whitespaces are ignored, and most statements are terminated by a semicolon.

### Variables:

Variables are declared with `let`. All variables are **mutable** and can be reassigned to **a value of the same type**. Variable types are inferred at declaration.

```bash
let x = 42;
x = 43;
```

Variables can be **64-bit signed integers**, or **boolean**.

### Control Flow:

Control flow is handled with `if` statements and `while` loops. The bodies are introduced by a colon `:` and wrapped in a block `{ ... }`.

```bash
if x > 0: {
    exit x;
}

while x <= 100: {
    x = x + 1;
}
```

### Expressions:

- **Integer arithmetic**: `+`, `-`, `*`, `/`
- **Relational operators**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Unary operators**: `-`, `+`, `!` (logical NOT)

Operator precedence and associativity follow classical C-like rules.

### Functions:

Functions are declared with `fn` and can be nested within other functions. They do not support parameters or return
values.

```bash
fn fooBar: {
    let x = 42;
    exit x;
}
```

They are called with `fooBar()`. Every function has return type `empty`.

### Scoping:

Each block, delimited with `{` `}`, creates a new scope. Shadowing is not permitted, even for disjoint scopes.

```bash
{
  let x = 42;
}

{
  let x = 43; # Error: redeclaration of symbol `x`
}

x = 41; # Error: assignment to undeclared variable `x`
```

### Comments:
Single-line comments start with `#` and extend to the end of the line.

```bash
# This is a comment
```

### Identifiers & Literals:
- **Identifiers** start with a letter, followed by any sequence of letters and digits (`foo1Bar2`).
- **Literals:**
  - **Non-negative integer:** `42`, `0`
  - **Boolean:** `true`, `false`

## Example Program

The following program returns 0 if `integer` is prime, its smallest prime divisor otherwise (or 1 if `integer` is less than 2).

```bash
let integer = 8000000011;

let isPrime = true;
let smallestDivisor = 1;
fn computeIsPrime: {
    if integer <= 1: {
        exit 1;
    }

    let curr = 2;
    while curr < integer: {
        if (integer / curr) * curr == integer: {
            isPrime = false;
            smallestDivisor = curr;
            curr = integer; # Equivalent to a break
        }
        curr = curr + 1;
    }
}

computeIsPrime();

if !isPrime: {
    exit smallestDivisor;
}

exit 0;
```

## Grammar

<details><summary>Formal EBNF Grammar</summary>

```
program ::= { statement }

statement ::= block-statement
            | assignment
            | declaration-assignment
            | function-declaration
            | if-statement
            | while-statement
            | exit-statement
            | expression-statement
            | comment

block-statement ::= '{' { statement } '}'

assignment ::= identifier '=' expression ';'

declaration-assignment ::= 'let' identifier '=' expression ';'

function-declaration ::= 'fn' identifier '(' ')' ':' block-statement

if-statement ::= 'if' expression ':' block-statement

while-statement ::= 'while' expression ':' block-statement

exit-statement ::= 'exit' expression ';'

expression-statement ::= expression ';'

comment ::= '#' { character }

expression ::= relational-expression

relational-expression ::= additive-expression
                        | additive-expression ("==" | "!=" | "<" | "<=" | ">" | ">=") additive-expression

additive-expression ::= multiplicative-expression
                      | additive-expression ('+' | '-') multiplicative-expression

multiplicative-expression ::= unary-expression
                            | multiplicative-expression ('*' | '/') unary-expression

unary-expression ::= primary-expression
                   | unary-op primary-expression

primary-expression ::= literal
                     | identifier
                     | function-call
                     | '(' expression ')'

function-call ::= identifier '(' ')'

unary-op ::= '-' | '+' | '!'

identifier ::= letter { letter | digit }

literal ::= integer-literal

integer-literal ::= digit { digit }
```

</details>