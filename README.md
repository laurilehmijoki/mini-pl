
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

var z : int := 3;
var foo : int := 1 + "";
print foo;
baz

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

var foo : int := 2;
var foo : int := 3;

```

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

var z : string := 3;
var foo : int := 1 + z;
print foo;

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

var z : int := 3;
var foo : string := z;
print foo;

```

### A correct program with integer arithmetics

```
var z : int := 1 + 2 * 3 * 4;
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


## Development

* Install sbt (a Scala build tool): http://www.scala-sbt.org/download.html

### Tests

```
sbt test
```

### Generate README

`./generate_readme`

## Author

Lauri Lehmijoki

## License

MIT

