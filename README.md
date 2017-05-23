
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


* compilation ___fails___
  * error: List(ParserNotFound(List(IdentifierToken(baz))))
          

### A program with duplicate var declarations

```
var foo : int := 2;
var foo : int := 3;
```


* compilation ___fails___
  * error: List(IdentifierAlreadyDeclared(IdentifierToken(foo),VarDeclaration(IdentifierToken(foo),IntTypeKeyword(int),OperandNode(IntToken(2)))))
          

### A program where the user assigns an integer into string

```
var z : string := 3;
var foo : int := 1 + z;
print foo;
```


* compilation ___fails___
  * error: List(IncompatibleTypes(OperandNode(IntToken(3)),class frontend.Token$IntToken,class frontend.Token$StringToken), InvalidExpression(OperatorNode(Plus(+),OperandNode(IntToken(1)),OperandNode(IdentifierToken(z)))))
          

### A program where the user assigns the value of the integer identifier to a string identifier

```
var z : int := 3;
var foo : string := z;
print foo;
```


* compilation ___fails___
  * error: List(IncompatibleTypes(OperandNode(IdentifierToken(z)),class frontend.Token$IntToken,class frontend.Token$StringToken))
          

### A correct program with integer arithmetics

```
var z : int := 1 + 2 * 3 * 4;
var foo : int := 1 + z;
print foo;
```


* compilation __succeeds__
* standard output is
```
26
```
* interpretation results in the following symbol table
  * z -> IntegerValue(25)
  * foo -> IntegerValue(26)

### A correct program with string concatenation

```
var z : string := "foo";
var foo : string := z + "bar";
print foo;
```


* compilation __succeeds__
* standard output is
```
foobar
```
* interpretation results in the following symbol table
  * z -> StringValue(foo)
  * foo -> StringValue(foobar)


## Author

Lauri Lehmijoki

## License

MIT

