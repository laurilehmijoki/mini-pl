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
}
