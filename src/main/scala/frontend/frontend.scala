package frontend

object Frontend {

  def main(args: Array[String]): Unit = {
    val tokens = Token.tokenize(
      """
|var fooBar : int := 1 + 2;
|var baz : int := 42;
|var hello % int := 99;
|:= hello % int := 99;
|string hello % int := 99;
|var hello : int := b;
      """.stripMargin.trim)

    val errorsOrStatements = StatementSequence.parse(tokens)
    println(errorsOrStatements)
  }
}

