package interpreter

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files
import javax.tools.ToolProvider

import frontend.Token.{ExpressionToken, IntTypeKeyword, StringTypeKeyword}
import frontend._

object Interpreter {
  def statementsToJava(statements: Seq[StatementSequence]): Option[String] =
    statements.foldLeft(None: Option[String]) { (javaSrc, statement) =>
      def asInfix(expression: AstNode): String = ast.toInfix(
        Token.tokenize(ast.toPostfix(expression)) collect {
          case e: ExpressionToken => e
        }
      )
      val statementInJava = statement match {
        case p: Print => s"System.out.println(${asInfix(p.expression)});" :: Nil
        case v: VarDeclaration =>
          (v.typeKeyword match {
            case IntTypeKeyword => "int"
            case StringTypeKeyword => "String"
          }) ::
          v.identifierToken.token ::
          "=" ::
          asInfix(v.expression) :: ";" :: Nil
      }
      Some(
        s"""
           |${javaSrc.getOrElse("")}
           |${statementInJava.mkString(" ")}
           |""".stripMargin)
    }

  def main(args: Array[String]) = {
    val tokens = Token.tokenize(
      """
        |var z : int := 1;
        |var foo : int := 1 + 3 * 3 - 5 - 5 + 1 + 2 / z;
        |print foo;
        |""".stripMargin)

    val parseResult = StatementSequence.parse(tokens)
    val javaStatements = statementsToJava(
      parseResult.map {_.right.get}
    ).get

    val root = new File(System.getProperty("java.io.tmpdir"))
    val sourceFile = File.createTempFile("Test", ".java", root)
    val className = sourceFile.getName.replaceAll(".java$", "")
    val javaSrc =
      s"""

public class $className {
    static {
        $javaStatements
    }
}
      """
    Files.write(sourceFile.toPath, javaSrc.getBytes("utf-8"))

    ToolProvider.getSystemJavaCompiler.run(null, null, null, sourceFile.getPath)

    val classLoader = URLClassLoader.newInstance(Seq(root.toURI.toURL).toArray)
    Class.forName(className, true, classLoader)

    println(javaSrc)
  }
}
