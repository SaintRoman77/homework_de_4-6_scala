object TaskA {
  def main(args: Array[String]): Unit = {
    helloPrint("Hello, Scala!")
  }

  private def helloPrint(str: String): Unit = {
    println(str)
    println(str.reverse)
    val strLowCase = str.toLowerCase().replace("!","")
    println(strLowCase + " and goodbye python!")
  }
}
