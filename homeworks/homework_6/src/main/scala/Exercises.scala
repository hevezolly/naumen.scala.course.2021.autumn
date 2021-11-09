import scala.collection.mutable.ArrayBuffer

object Exercises {

  def reverse[T](seq: Seq[T]): Seq[T] = {
    var newSeq: List[T] = List()
    seq.foreach(e => newSeq = e :: newSeq)
    newSeq
  }
  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = fibonacci(idx)(idx)

  def fibonacci(idx: Int): Seq[Int] = {
    val fib = ArrayBuffer.fill(idx + 1)(0)
    fib(1) = 1
    fib.indices.drop(2).foreach(i => fib(i) = fib(i-1) + fib(i-2))
    fib
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.toUpperCase.map(s => MORSE.contains(s.toString) match {
    case true => MORSE(s.toString)
    case false => s.toString
  }).mkString(" ")


  def wordReverse(text: String): String = {

    var newText = ""
    var curr_word = ""
    var isText = text(0).isLetter

    def wordReverse(string: String): String = {
      var word = reverse(string).mkString("").toLowerCase
      if (string(0).isUpper)
        word = word(0).toUpper + word.drop(1)
      word
    }

    def addWord(): Unit = newText += (if(isText) wordReverse(curr_word) else curr_word)

    text.foreach(s => {
      if (s.isLetter == isText) {
        curr_word += s
      } else {
        addWord()
        isText = !isText
        curr_word = s.toString
      }
    })
    addWord()
    newText
  }

}
