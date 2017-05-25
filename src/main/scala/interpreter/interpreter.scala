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

  type SymbolTable = Map[IdentifierToken, SymbolValue]

  def visit(ast: Ast, systemOut: PrintStream, symbols: SymbolTable = Map()): SymbolTable =
    ast match {
      case EmptyNode => symbols
      case ast: AstNode =>
        def updateSymbol(identifierToken: IdentifierToken, expression: Expression) =
          symbols + (identifierToken -> evaluate(expression, symbols))
        val symbolsAfterStatement = ast.statement match {
          case v: VarDeclaration =>
            updateSymbol(v.identifierToken, v.expression)
          case v: VarAssignment =>
            updateSymbol(v.identifierToken, v.expression)
          case p: Print =>
            val symbolValue = evaluate(p.expression, symbols)
            symbolValue match {
              case i: IntegerValue => systemOut.println(i.value)
              case s: StringValue => systemOut.println(s.value)
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
            symbols.getOrElse(identifier, throw new RuntimeException(s"Cannot find symbol $identifier"))
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
}
