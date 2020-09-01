package collatz

object App {
   def main(args: Array[String]) {
      val start = args(0)
      val startInt = Integer.valueOf(start)
      val startBinary = Binary(startInt)
      val next = startBinary.reduce
   }
}
