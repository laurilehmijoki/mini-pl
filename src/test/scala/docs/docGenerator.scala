package docs

object docGenerator {
  def main(args: Array[String]): Unit = {
    val readmeMarkdown =
      s"""
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

${samples.samplePrograms.programs.map(Function.tupled({ (description, program, _) =>
s"""
### $description

```
${program.trim()}
```
"""
})).mkString}
## Author

Lauri Lehmijoki

## License

MIT
"""
    println(readmeMarkdown)
  }

}
