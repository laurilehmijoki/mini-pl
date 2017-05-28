package interpreter

import java.io.PrintStream

import frontend.Token._
import frontend._
import utils.{BashShell, FormattingContext, errorReporter}

sealed trait SymbolValue
case class IntegerValue(value: Int) extends SymbolValue
case class StringValue(value: String) extends SymbolValue

object interpreter {

  def interpret(program: String, systemOut: PrintStream)(implicit formattingContext: FormattingContext): Int = {
    val verifiedProgram = frontendHelper.verify(program)
    val exitStatus = verifiedProgram.right.map(program => astUtils.build(program.statements)) match {
      case Left(err: Seq[CompilationError]) =>
        val errorReport = errorReporter.createErrorReport(program, err)
        println(s"${errorReport.headlines.mkString("\n")}\n${errorReport.highlightedSourceCode}")
        1
      case Right(ast) =>
        visit(ast, systemOut)
        0
    }
    exitStatus
  }

  def interpret(verifiedProgram: VerifiedProgram, systemOut: PrintStream): SymbolTable = {
    val ast = astUtils.build(verifiedProgram.statements)
    visit(ast, systemOut)
  }

  type SymbolTable = Map[String, SymbolValue]
  def updateSymbol(symbols: SymbolTable, identifierToken: IdentifierToken, symbolValue: SymbolValue): SymbolTable =
    symbols + (identifierToken.token -> symbolValue)
  def updateWithExpression(symbols: SymbolTable, identifierToken: IdentifierToken, expression: Expression): SymbolTable =
    updateSymbol(symbols, identifierToken, evaluate(expression, symbols))

  def visit(ast: Ast, systemOut: PrintStream, symbols: SymbolTable = Map()): SymbolTable =
    ast match {
      case EmptyNode => symbols
      case ast: AstNode =>

        val symbolsAfterStatement: SymbolTable = ast.statement match {
          case f: ForLoop =>
            val (from, to) = (evaluate(f.from, symbols), evaluate(f.to, symbols)) match {
              case (IntegerValue(fromInt), IntegerValue(toInt)) => (fromInt, toInt)
              case (a, b) => throw new RuntimeException(s"Cannot loop from $a to $b")
            }
            (from until to).foldLeft(symbols) { (symbolsMemo: SymbolTable, step: Int) =>
              updateSymbol(symbolsMemo, f.identifierToken, IntegerValue(step))
            }
          case v: VarDeclaration =>
            val symbolValue = v.expression
              .map(expr => evaluate(expr, symbols))
              .getOrElse(defaultValue(v.typeKeyword))
            updateSymbol(symbols, v.identifierToken, symbolValue)
          case v: VarAssignment =>
            updateWithExpression(symbols, v.identifierToken, v.expression)
          case p: Print =>
            val symbolValue = evaluate(p.expression, symbols)
            symbolValue match {
              case i: IntegerValue => systemOut.print(i.value)
              case s: StringValue => systemOut.print(s.value)
            }
            symbols
        }

        ast.children.foldLeft(symbolsAfterStatement) { (syms, node) =>
          visit(node, systemOut, syms)
        }
    }

  def evaluate(rootNode: Expression, symbols: SymbolTable): SymbolValue =
    rootNode match {
      case operandNode: OperandNode =>
        operandNode.operandToken match {
          case intToken: IntToken => IntegerValue(intToken.intValue)
          case stringToken: StringToken => StringValue(stringToken.containedString)
          case identifier: IdentifierToken =>
            symbols.getOrElse(identifier.token, throw new RuntimeException(s"Cannot find symbol $identifier"))
        }
      case operatorNode: OperatorNode =>
        (evaluate(operatorNode.left, symbols), evaluate(operatorNode.right, symbols)) match {
          case (IntegerValue(left), IntegerValue(right)) =>
            val intResult = operatorNode.operatorToken match {
              case Plus(_) => left + right
              case Minus(_) => left - right
              case Divide(_) => left / right
              case Multiply(_) => left * right
            }
            IntegerValue(intResult)
          case (StringValue(left), StringValue(right)) =>
            val result = operatorNode.operatorToken match {
              case Plus(_) => left + right
              case Minus(_) | Divide(_) | Multiply (_) =>
                s"Cannot apply $operatorNode on (${operatorNode.left}, ${operatorNode.right})"
            }
            StringValue(result)
          case _ =>
            throw new RuntimeException(s"Cannot apply $operatorNode on (${operatorNode.left}, ${operatorNode.right})")
        }
    }

  def defaultValue(typeKeyword: TypeKeyword): SymbolValue =
    typeKeyword match {
      case _: StringTypeKeyword => StringValue("")
      case _: IntTypeKeyword => IntegerValue(0)
    }
}
