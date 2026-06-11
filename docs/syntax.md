# Grammar

- {a} means zero of more repetitions of a
- [a] means zero or one occurrence of a
- a | b means either a or b
- 'a' and "a" mean the string a
- ~['a' 'b'] means any character except a and b

<details><summary>Formal EBNF Grammar</summary>

```bnf
compilation-unit ::= { external-function-declaration } { function-definition } EOF

function-signature ::= identifier '(' parameter-list ')' [ '->' type-specifier ]

external-function-declaration ::= 'extern' 'fn' function-signature ';'

function-definition ::= [ 'export' ] 'fn' function-signature ':' block-statement

statement ::= block-statement
            | variable-definition
            | assignment
            | if-statement
            | while-statement
            | break-statement
            | continue-statement
            | return-statement
            | exit-statement
            | expression-statement
            | comment

block-statement ::= '{' { statement } '}'

type-specifier ::= 'int' | 'int8' | 'int16' | 'int32' | 'int64'
                 | 'bool'
                 | 'char'
                 | array-type

array-type ::= '[' type-specifier ';' integer-literal ']'

variable-definition ::= 'let' [ 'mut' ] identifier [ ':' type-specifier ] '=' expression ';'

assignment ::= expression ('=' | '+=' | '-=' | '*=' | '/=') expression ';'

parameter-list ::= [ parameter-declaration { ',' parameter-declaration } ]

parameter-declaration ::= [ 'mut' ] identifier ':' type-specifier

if-statement ::= 'if' expression ':' block-statement { elif-clause } [ else-clause ]

elif-clause ::= 'elif' expression ':' block-statement

else-clause ::= 'else' ':' block-statement

while-statement ::= 'while' expression ':' block-statement

break-statement ::= 'break' ';'

continue-statement ::= 'continue' ';'

return-statement ::= 'return' ';'
                   | 'return' expression ';'

exit-statement ::= 'exit' expression ';'

expression-statement ::= expression ';'

comment ::= '#' { ~['\n'] }

expression ::= logical-expression

logical-expression ::= comparison-expression
                     | or-chain-expression
                     | and-chain-expression

or-chain-expression ::= or-chain-expression '||' comparison-expression

and-chain-expression ::= and-chain-expression '&&' comparison-expression

comparison-expression ::= additive-expression
                        | additive-expression ('==' | '!=' | '<' | '<=' | '>' | '>=') additive-expression

additive-expression ::= multiplicative-expression
                      | additive-expression ('+' | '-') multiplicative-expression

multiplicative-expression ::= unary-expression
                            | multiplicative-expression ('*' | '/') unary-expression
                            
unary-expression ::= postfix-expression
                    | unary-op postfix-expression
                    
unary-op ::= '-' | '+' | '!'

postfix-expression ::= primary-expression
                     | postfix-expression '[' expression ']'
                                        
primary-expression ::= literal
                     | identifier
                     | function-call
                     | '(' expression ')'

literal ::= integer-literal | boolean-literal | char-literal | array-literal | repeat-array-literal

array-literal ::= '[' expression-list ']'

repeat-array-literal ::= '[' expression ';' integer-literal ']' 

function-call ::= identifier '(' expression-list ')'

expression-list ::= [ expression { ',' expression } ]

identifier ::= letter { letter | digit | '_' }

integer-literal ::= digit { digit }

boolean-literal ::= 'true' | 'false'

char-literal ::= SQ (~[SQ '\' LF CR TAB] | quote-escape | escape-sequence) SQ

quote-escape ::= '\' (SQ | DQ)

escape-sequence ::= '\' ('\' | 'n' | 'r' | 't' | '0')

letter ::= 'a'..'z' | 'A'..'Z'

digit ::= '0'..'9'

SQ ::= "'"

DQ ::= '"'
```

</details>
