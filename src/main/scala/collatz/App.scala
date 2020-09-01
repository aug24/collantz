package collatz

object App {
   def main(args: Array[String]) {
      val start = args(0)
      val startBinary = try {
         Binary(Integer.valueOf(start))
      } catch {
         case e:NumberFormatException => Binary(start)
      }
      val next = startBinary.reduce
   }
}
