package collatz

object Binary {
  def apply(s: String): Binary = {
     Binary(s.getBytes.map(b => b == 'x').toList)
  }
  def apply(i: Int, p: Int = 1): Binary = {
     if (i>=p) {
       apply(i, 2*p)
     } else {
       Binary(constructBinary(i, p/2).reverse)
     }
  }
  def constructBinary(i: Int, p: Int):List[Boolean] = {
    val bit = i>=p
    val remainder = if (bit) i-p else i
    if (p==1) {
      List(bit)
    } else {
      bit :: constructBinary(remainder, p/2)
    }
  }
  def trim(list: List[Boolean], trimVal: Boolean): List[Boolean] = {
    list match {
      case head :: tail if head == trimVal => trim(tail, trimVal)
      case _ => list
    }
  }
}

case class Binary(val list: List[Boolean]) {
  def reduce(): Binary = {
    this.prettyPrint()
    val one = list :+ false
    val doublePlusOne  = true :: list
    val triplePlusOne = one.zip(doublePlusOne)
    val newList = Binary.trim(sortOfFold(triplePlusOne, false), false)
    newList match {
       case head :: Nil => Binary(newList)
       case _ =>  Binary(newList).reduce
    }
  }
  def sortOfFold(l: List[(Boolean, Boolean)], carry: Boolean): List[Boolean] = {
    //println(s"$carry ... $l")
    l match {
      case head :: tail if (head._1 && head._2) => carry :: sortOfFold(tail, true)
      case head :: tail if (head._1 || head._2) => !carry :: sortOfFold(tail, carry)
      case head :: tail if (!head._1 && !head._2) => carry :: sortOfFold(tail, false)
      case Nil if carry => List(carry)
      case Nil if !carry => Nil
    }
  }
  def prettyPrint(i: Int=0, column: Int=1) : Unit = list match {
    case Nil => println(s" ($i)")
    case head :: tail if head => {
      print("x")
      Binary(tail).prettyPrint(i+1*column, column*2)
    }
    case head :: tail if !head => {
      print(".")
      Binary(tail).prettyPrint(i, column*2)
    }
  }
}
 
   
