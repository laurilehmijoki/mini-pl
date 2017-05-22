package interpreter

import frontend.Token._
import frontend._

object interpreter {
  def main(args: Array[String]) = {
    val program = """
                    |var z : int := 3;
                    |var foo : int := 1 + z;
                    |print foo;
                    |""".stripMargin


    System.exit(interpret(program))
  }

  def interpret(program: String): Int = {
    val tokens = Token.tokenize(program)

    val parseResult = StatementSequence.parse(tokens).foldLeft(Right(Nil): Either[Seq[ParseError], Seq[StatementSequence]]) {
      (memo, item) => item match {
        case Left(errors) =>
          val previousErrors = memo.left.getOrElse(Nil)
          Left(previousErrors :+ errors)
        case Right(statementSequences) =>
          memo.right.map { _ :+ statementSequences}
      }
    }
    val exitStatus = parseResult.right.map(astUtils.build) match {
      case Left(err) =>
        println(err)
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
            println(evaluate(p.expression, symbols))
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
          case identifier: IdentifierToken =>
            symbols.getOrElse(identifier, throw new RuntimeException(s"Cannot find symbol $identifier"))
        }
      case operatorNode: OperatorNode =>
        (evaluate(operatorNode.left, symbols), evaluate(operatorNode.right, symbols)) match {
          case (IntegerValue(left), IntegerValue(right)) =>
            val intResult = operatorNode.operatorToken match {
              case Plus => left + right
              case Minus => left - right
              case Divide => left / right
              case Multiply => left * right
            }
            IntegerValue(intResult)
          case _ =>
            throw new RuntimeException(s"Cannot apply $operatorNode on (${operatorNode.left}, ${operatorNode.right})")
        }

    }
}
