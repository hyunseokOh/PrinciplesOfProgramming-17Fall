package pp201701.hw4test
import pp201701.hw4.Main._
import pp201701.hw4.Data.DataBundle._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  {
    val intEq: Int => Int => Boolean = { x => y => (x == y) }
    val d0 = Problem1.IterDictImpl.empty[Int, Int](intEq)
    val d1 = d0.add(1,1)
    val d2 = d1.add(2, 0)
    val d3 = d2.add(3, 0)
    val d4 = d3.add(2,2)
    val d5= d4.add(4,4)
    val d6=d5.add(3,3)
    val d7=d6.add(5,5)
    val d8=d7.add(6,6)

    print_result(
      d8.find(5) match {
        case Some(n) => n == 5
        case _ => false
      }
    )

    print_result(Problem1.sumElements(d8)==21)
  }

  // Problem 2
  {
    val x = List("A", "B", "C")
    val bx = new Problem2.BiIterableList(x)

    print_result(
      bx.biIter.getValue match {
        case Some(x) => x == "A"
        case _ => false
      }
    )

    println(bx.biIter.getValue) // A
    println(bx.biIter.getNext.getValue) // B
    println(bx.biIter.getNext.getNext.getValue) // C
    println(bx.biIter.getNext.getPrev.getValue) // A
    println(bx.biIter.getPrev.getPrev.getNext.getValue) // A
    println(bx.biIter.getNext.getNext.getNext.getPrev.getValue) // C
    println(bx.biIter.getNext.getNext.getNext.getNext.getNext.getPrev.getValue) // C

  }
}
