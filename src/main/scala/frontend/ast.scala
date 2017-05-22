package frontend

import frontend.Token._

sealed trait Ast

object EmptyNode extends Ast
case class AstNode(statement: StatementSequence, children: Seq[AstNode] = Nil) extends Ast

sealed trait SymbolValue
case class IntegerValue(value: Int) extends SymbolValue

object astUtils {
  def build(statements: Seq[StatementSequence]): Ast = {
    def toNode(statement: StatementSequence): AstNode =
      statement match {
        case v: VarDeclaration => AstNode(v, children = Nil)
        case p: Print => AstNode(p, children = Nil)
      }
    statements match {
      case head +: tail => toNode(head).copy(children = tail.map(toNode))
      case Nil => EmptyNode
    }
  }

  def main(args: Array[String]) = {
    val program = """
                    |var z : int := 3;
                    |var foo : int := 1 + z;
                    |print foo;
                    |""".stripMargin

    val tokens = Token.tokenize(program)

    val parseResult = StatementSequence.parse(tokens).foldLeft(Right(Nil): Either[Seq[ParseError], Seq[StatementSequence]]) {
      (memo, item) => item match {
        case Left(errors) => memo.left.map { _ :+ errors}
        case Right(statementSequences) => memo.right.map { _ :+ statementSequences}
      }
    }
    parseResult.right.map(build) match {
      case Left(err) =>
        println(err)
        System.exit(1)
      case Right(ast) =>
        visit(ast)
    }
    ()
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
