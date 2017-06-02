
# A mini-pl compiler

## Context-free grammar

```
<prog>   ::=  <stmts>
<stmts>  ::=  <stmt> ";" ( <stmt> ";" )*
<stmt>   ::=  "var" <var_ident> ":" <type> [ ":=" <expr> ]
          |   <var_ident> ":=" <expr>
          |   "for" <var_ident> "in" <expr> ".." <expr> "do"
                 <stmts> "end" "for"
          |   "read" <var_ident>
          |   "print" <expr>
          |   "assert" "(" <expr> ")"

<expr>   ::=  <opnd> <op> <opnd>
          |   [ <unary_op> ] <opnd>
<opnd>   ::=  <int>
          |   <string>
          |   <var_ident>
          |   "(" expr ")"

<type>   ::=  "int" | "string" | "bool"
<var_ident> ::= <ident>

<reserved keyword> ::=
             "var" | "for" | "end" | "in" | "do" | "read" |
             "print" | "int" | "string" | "bool" | "assert"
```

Source: https://www.cs.helsinki.fi/u/vihavain/k16/Compilers/project/miniplsyntax_2016.html – thank you for the inspiring and educational page Mr Vihavainen!

## Examples of results after lexical, syntactic and semantic analyses


### A program with unrecognised statement

```
var z : int := 3;
var foo : int := 1 + "";
print foo;
baz
```


* compilation fails
* error:
```
Syntax error: Cannot find parser for tokens List(IdentifierToken(baz))
```

### A program with duplicate var declarations

```
var foo : int := 2;
var foo : int := 3;
```


* compilation fails
* error:
```
Syntax error: Token is already declared
```

### A program with var reassignment

```
var foo : int := 2;
foo := 3;
```


* compilation succeeds
* standard output is

```

```

* interpretation results in the following symbol table
  * `foo` -> `3`

### A program with illegal var reassignment

```
foo := 3;
```


* compilation fails
* error:
```
Syntax error: Identifier "foo" is not declared
```

### A program with var default value

```
var intIdentifier : int;
var stringIdentifier : string;
```


* compilation succeeds
* standard output is

```

```

* interpretation results in the following symbol table
  * `intIdentifier` -> `0`
  * `stringIdentifier` -> ``

### A program where the user assigns an integer into string

```
var z : string := 3;
var foo : int := 1 + z;
print foo;
```


* compilation fails
* error:
```
Type error: Expected StringToken but got IntToken in expression OperandNode(IntToken(3))
Invalid expression: OperatorNode(Plus(+),OperandNode(IntToken(1)),OperandNode(IdentifierToken(z)))
```

### A program where the user assigns the value of the integer identifier to a string identifier

```
var z : int := 3;
var foo : string := z;
print foo;
```


* compilation fails
* error:
```
Type error: Expected StringToken but got IntToken in expression OperandNode(IdentifierToken(z))
```

### A correct program with integer arithmetics

```
var z : int := 1 + 2*3 * 4;
var foo : int := 1 + z;
print foo;
```


* compilation succeeds
* standard output is

```
26
```

* interpretation results in the following symbol table
  * `z` -> `25`
  * `foo` -> `26`

### A correct program with string concatenation

```
var z : string := "foo";
var foo : string := z + "bar";
print foo;
```


* compilation succeeds
* standard output is

```
foobar
```

* interpretation results in the following symbol table
  * `z` -> `foo`
  * `foo` -> `foobar`

### A program with for loop

```
var z : int;
for z in 0..5 do
  print z;
end for;
```


* compilation succeeds
* standard output is

```
01234
```

* interpretation results in the following symbol table
  * `z` -> `4`

### A program with empty for loop body

```
var z : int;
for z in 0..5 do
end for;
```


* compilation fails
* error:
```
Syntax error: The for loop body may not be empty
```

### A program with for loop from string to int

```
var z : int;
for z in "test"..5 do
  print z;
end for;
```


* compilation fails
* error:
```
Type error: Expected IntToken but got StringToken in expression OperandNode(StringToken("test"))
```

### A program with reassignment of the for loop control variable

```
var z : int;
for z in 2..3 do
  z := 5;
end for;
```


* compilation fails
* error:
```
Syntax error: The for loop control variable IdentifierToken(z) may not be reassigned
```

### A program with var declaration within the for loop

```
var z : int;
for z in 2..3 do
  var y : int;
end for;
```


* compilation fails
* error:
```
Syntax error: For loop may not contain var declarations
```

### A program with stdin read

```
var z : int;
read z;
```


* compilation succeeds
* standard output is

```

```

* interpretation results in the following symbol table
  * `z` -> `42`


## Development

* Install sbt (a Scala build tool): http://www.scala-sbt.org/download.html

### Tests

[![Build Status](https://travis-ci.org/laurilehmijoki/mini-pl.svg?branch=master)](https://travis-ci.org/laurilehmijoki/mini-pl)

```
sbt test
```

### Generate README

`./generate_readme`

## Author

Lauri Lehmijoki

## License

MIT

