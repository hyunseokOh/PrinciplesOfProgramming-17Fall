package pp201701.hw1test
import pp201701.hw1.Main._

object Test extends App {
  def print_result(b:Boolean) : Unit =
    if (b) println("O") else println("X")

  // Problem 1
  print_result(ppascal(2,1) == 3)

  // Problem 2
  print_result(fibA(4) == 3)
  print_result(fibB(4) == 3)
  print_result(fibC(4) == 3)
}
