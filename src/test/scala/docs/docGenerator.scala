package docs

import java.io.{ByteArrayOutputStream, PrintStream}

import frontend.Token.IdentifierToken
import frontend.{CompilationError, frontendHelper}
import interpreter.SymbolValue
import interpreter.interpreter._

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

$programsToMarkdown

## Author

Lauri Lehmijoki

## License

MIT
"""
    println(readmeMarkdown)
  }

  def programsToMarkdown = {
    samples.samplePrograms.programs.map(Function.tupled({ (description, program, _) =>
      val failureReportOrSymbolTableAndStdout: Either[Seq[CompilationError], (SymbolTable, String)] = frontendHelper.verify(program)
        .right
        .map { verifiedProgram =>
          val baos = new ByteArrayOutputStream()
          val symbolTable = interpret(verifiedProgram, new PrintStream(baos))
          (symbolTable, new String(baos.toByteArray, "utf-8"))
        }

      s"""
### $description

```
${program.trim()}
```

${
        failureReportOrSymbolTableAndStdout.fold(
          error =>
            s"""
               |* compilation fails
               |  * error: $error
          """.stripMargin,
          Function.tupled((symbolTable: SymbolTable, stdOut: String) =>
            s"""
               |* compilation succeeds
               |* standard output is
               |```
               |$stdOut```
               |* interpretation results in the following symbol table
               |${
              symbolTable.foldLeft(None: Option[String]) { (markdown, symbolTableEntry) =>
                val entryAsMarkdown =
                  s"  * ${symbolTableEntry._1.token} -> ${symbolTableEntry._2}"
                markdown match {
                  case None =>
                    Some(entryAsMarkdown)
                  case Some(previousEntry) => Some(
                    previousEntry :: entryAsMarkdown :: Nil mkString "\n"
                  )
                }
              }.getOrElse("  * [empty symbol table]")
            }""".stripMargin
          ))
      }
"""
    })).mkString
  }
}