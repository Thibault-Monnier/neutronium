# Neutronium Language

Neutronium is a lightweight C-like programming language that supports basic constructs such as variables, functions, and
control flow.

## Features & Syntax

Whitespaces are ignored, and most statements are terminated by a semicolon.

### Variables:

Variables are declared with `let` and are **immutable by default**. To make it mutable, use `let mut` instead. Mutable
variables can be reassigned to **a value of the same type**.
Variable types are **inferred at declaration**, or can be explicitly declared with `let <name>: <type> = <value>;`.

```bash
let x = true;
let mut y: int = 42;
y = 43;
```

Variables can be **integers** or **booleans**.

### Control Flow:

- `if`, `elif`, and `else` **statements** do not wrap their conditions in parentheses. Each condition is followed by a
  colon `:` and a block delimited by braces `{` `}`.
- `while` **loops** use the same syntax as `if` **statements**.
- `break;` and `continue;` **statements** are used to exit or skip the current iteration of a loop, respectively.
- `exit` **statements** terminate the program with the given **exit code**. The exit code is an **integer expression**.

```bash
if x > 0: {
    while true: {
        break; # Loop stops immediately
    }
    exit 1;
} elif x == 0: {
    exit 0;
} else: {
    while x <= 42: {
        x = x + 1;
        continue; # Unnecessary in this case
    }
}
```

### Expressions:

- **Integer arithmetic**: `+`, `-`, `*`, `/`
- **Relational operators**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Unary operators**: `-`, `+`, `!` (logical NOT)

Operator precedence and associativity follow classical C-like rules.

### Functions:

Functions are declared with `fn` and can be nested within other functions. To specify a function's return type, use `->` followed by the type; if not specified, the return type is `void`. 
Currently, functions do not support parameters.

```bash
fn fooBar() -> void: {
    let x = 42;
    exit x;
}
```

They are called with `fooBar()`. Functions can have any return type.

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

### Types:

- **Integer**: 64-bit signed integer
- **Boolean**: `true` or `false`
- **Void**: The return type of _all_ functions (since return values are not yet supported)

## Example Program

The following program returns 0 if `integer` is prime, its smallest prime divisor otherwise (or 1 if `integer` is less
than 2).

```bash
let integer: int = 8000000011;

let mut isPrime = true;
let mut smallestDivisor = 1;
fn computeIsPrime() -> void: {
    if integer <= 1: {
        exit 1;
    }

    let mut curr = 2;
    while curr < integer: {
        if (integer / curr) * curr == integer: {
            isPrime = false;
            smallestDivisor = curr;
            break;
        } else: {
            curr = curr + 1;
        }
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
program ::= { statement } EOF

statement ::= block-statement
            | assignment
            | declaration-assignment
            | function-declaration
            | if-statement
            | while-statement
            | break-statement
            | continue-statement
            | exit-statement
            | expression-statement
            | comment

block-statement ::= '{' { statement } '}'

type-specifier ::= 'int' | 'bool' | 'void'

assignment ::= identifier '=' expression ';'

declaration-assignment ::= 'let' [ 'mut' ] identifier [ ':' type-specifier ] '=' expression ';'

body ::= statement | block-statement

function-declaration ::= 'fn' identifier '(' ')' [ '->' type-specifier ] ':' body

if-statement ::= 'if' expression ':' body { elif-clause } [ else-clause ]

elif-clause ::= 'elif' expression ':' body

else-clause ::= 'else' ':' body

while-statement ::= 'while' expression ':' body

break-statement ::= 'break' ';'

continue-statement ::= 'continue' ';'

exit-statement ::= 'exit' expression ';'

expression-statement ::= expression ';'

comment ::= '#' { any-character-except-newline }

expression ::= comparison-expression

comparison-expression ::= additive-expression
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

literal ::= integer-literal | boolean-literal

integer-literal ::= digit { digit }

boolean-literal ::= 'true' | 'false'

letter ::= 'a'..'z' | 'A'..'Z'

digit ::= '0'..'9'
```

</details>