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
      def asInfix(expression: Expression): String = expression.toInfix(
        Token.tokenize(expression.toPostfix(expression)) collect {
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
    interpret(
      """
        |var z : int := 3;
        |var foo : int := 1 + 3 * 3 - 5 - 5 + 1 + 2 / z;
        |print foo;
        |""".stripMargin
    ) match {
      case Left(err) =>
        println(err)
        System.exit(1)
      case Right(programEvaluator) =>
        programEvaluator()
    }
  }

  def interpret(program: String): Either[String, () => Unit] = {
    val tokens = Token.tokenize(program)

    val parseResult = StatementSequence.parse(tokens).foldLeft(Right(Nil): Either[Seq[ParseError], Seq[StatementSequence]]) {
      (memo, item) => item match {
        case Left(errors) => memo.left.map { _ :+ errors}
        case Right(statementSequences) => memo.right.map { _ :+ statementSequences}
      }
    }

    parseResult.right.map(statementsToJava) match {
      case Right(Some(javaStatements)) =>
        Right({ () =>
          val root = new File(System.getProperty("java.io.tmpdir"))
          val sourceFile = File.createTempFile("Test", ".java", root)
          sourceFile.deleteOnExit()
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
        })
      case Right(None) => Left("Could not construct Java statements")
      case Left(err) => Left(err.mkString("\n"))

    }
  }
}
