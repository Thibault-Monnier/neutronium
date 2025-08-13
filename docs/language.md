# Grammar

<details><summary>Formal EBNF Grammar</summary>

```
program ::= { const-definition } { external-function-declaration } { function-definition } EOF

function-signature ::= identifier '(' parameter-list ')' [ '->' type-specifier ]

external-function-declaration ::= 'extern' 'fn' function-signature ';'

function-definition ::= [ 'export' ] 'fn' function-signature ':' block-statement

const-definition ::= 'const' identifier [ ':' type-specifier ] '=' expression ';'

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

type-specifier ::= 'int'
                 | 'bool'
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

return-statement ::= 'return' expression ';'

exit-statement ::= 'exit' expression ';'

expression-statement ::= expression ';'

comment ::= '#' { any-character-except-newline }

expression ::= comparison-expression

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

literal ::= integer-literal | boolean-literal | array-literal

array-literal ::= '[' expression-list ']'

function-call ::= identifier '(' expression-list ')'

expression-list ::= [ expression { ',' expression } ]

identifier ::= letter { letter | digit | '_' }

integer-literal ::= digit { digit }

boolean-literal ::= 'true' | 'false'

letter ::= 'a'..'z' | 'A'..'Z'

digit ::= '0'..'9'
```

</details>
