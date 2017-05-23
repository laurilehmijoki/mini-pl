package interpreter

import frontend.Token._
import frontend._
import utils.errorReporter

sealed trait SymbolValue
case class IntegerValue(value: Int) extends SymbolValue
case class StringValue(value: String) extends SymbolValue

object interpreter {
  val programWithTwoErrors = """
                  |var z : int := 3;
                  |var foo : int := 1 + "";
                  |print foo;
                  |baz
                  |""".stripMargin

  val typeAssignmentError = """
                               |var z : string := 3;
                               |var foo : int := 1 + z;
                               |print foo;
                               |""".stripMargin

  val stringAndIntEvaluation = """
                              |var foo : int := 1 + "test";
                              |print foo;
                              |""".stripMargin

  val program = """
                               |var z : int := 3;
                               |var foo : int := 1 + z;
                               |print foo;
                               |""".stripMargin

  val printUndeclaredIdentifier = """
                  |var foo : int := 1 + z;
                  |print z;
                  |""".stripMargin

  val duplicateDeclaration = """
                                    |var foo : int := 2;
                                    |var foo : int := 3;
                                    |""".stripMargin

  val stringConcatenation = """
                               |var foo : string := "a" + "b";
                               |print foo;
                               |""".stripMargin

  def main(args: Array[String]) = {
    System.exit(interpret(typeAssignmentError))
  }

  def interpret(program: String): Int = {
    val verifiedProgram = frontendHelper.verify(program)
    val exitStatus = verifiedProgram.right.map(program => astUtils.build(program.statements)) match {
      case Left(err: Seq[CompilationError]) =>
        println(errorReporter.createErrorReport(program, err))
        1
      case Right(ast) =>
        visit(ast)
        0
    }
    exitStatus
  }

  type SymbolTable = Map[IdentifierToken, SymbolValue]

  def visit(ast: Ast, symbols: SymbolTable = Map()): SymbolTable =
    ast match {
      case EmptyNode => symbols
      case ast: AstNode =>
        val symbolsAfterStatement = ast.statement match {
          case v: VarDeclaration =>
            symbols + (v.identifierToken -> evaluate(v.expression, symbols))
          case p: Print =>
            val symbolValue = evaluate(p.expression, symbols)
            symbolValue match {
              case i: IntegerValue => println(i.value)
              case s: StringValue => println(s.value)
            }
            symbols
        }

        ast.children.foldLeft(symbolsAfterStatement) { (syms, node) =>
          visit(node, syms)
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
