package pp201701.hw4

import pp201701.hw4.Data.DataBundle._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 * ** When you give up a problem, use the commented code:
      ex.) If you want to give up the first problem,
        def push[A](stk: Stack[A])(a: A): Stack[A] = stk
 */

object Main {

  /* Problem 1
   Complete an implementation of 'IterDict'.

   IterDict represents a dictionary type.
   (See https://en.wikipedia.org/wiki/Associative_array for more information.)

   Briefly, you can store (key, value) pair into a dictionary,
   and search the stored value later from the associated key value.

   Here, your dictionary should be properly iterable.
   */

  object Problem1 {

    object IterDictImpl {
      //Write empty IterDict
      def empty[K, V](eqFunc: K => K => Boolean): IterDict[K, V] = new IterDictImpl(eqFunc)(Nil) // null
    }

    class IterDictImpl[K, V](eqFunc: K => K => Boolean)(val data: List[(K, V)])
      extends IterDict[K, V] {

      def getValue = data.headOption // None

      def getNext = new IterDictImpl(eqFunc)(data.tail) // null

      // When the given key already exists in this dictionary, overwrite the value.
      def add(k: K, v: V): IterDict[K, V] = {
        def _add(lst: List[(K, V)], k: K, v: V): List[(K, V)] = lst match {
          case Nil => List((k, v))
          case (key, value) :: lst2 =>
            if (eqFunc(k)(key)) ((k, v) :: lst2)
            else (key, value) :: _add(lst2, k, v)
        }

        new IterDictImpl(eqFunc)(_add(data, k, v))
      } // null

      // Return the associated value with the key. When there is no such key, return None.
      def find(k: K): Option[V] = data match {
        case Nil => None
        case (key, value) :: _ =>
          if (eqFunc(k)(key)) Some(value)
          else getNext.find(k)
      } // None
    }

    // Write a function that iterates through given iterator and sum it.
    def sumElements[K](xs: Iter[(K, Int)]): Int = xs.getValue match {
      case None => 0
      case Some((_, value)) => value + sumElements(xs.getNext)
    } // 0
  }

  // Problem 2
  object Problem2 {

    /*
     Define 'BiIterable' list and tree classes.
     These should provide bi-directional iteration, so you can freely move
     the iterator forward(getNext) or backward(getPrev) at any moment.

     For a BiIterableTree, the iterator should first visit the root, and then
     visit all nodes in the left subtree, and finally visit all nodes in the
     right subtree.
     (See preordering in https://en.wikipedia.org/wiki/Depth-first_search
     for more information.)

     At each end, your BiIter cannot go further, but should be able to go back
     to the reversed direction.

     For example:
     When you are on the first element,
     - getPrev.getValue gives None.
     - getPrev.getPrev.getValue also gives None.
     - getPrev.getPrev.getPrev.getPrev.getNext.getValue gives Some(first-element)

     Similarly, when you are on the last element,
     - getNext.getValue gives None.
     - getNext.getNext.getValue also gives None.
     - getNext.getNext.getNext.getNext.getPrev.getValue gives Some(last-element)

     You may create your own classes for this exercise.
    */
    sealed abstract class BiList[A] extends BiIter[A]
    case class BiNil[A](val rtl: List[A], val ltl : List[A]) extends BiIter[A] {
      def getValue = None

      def getNext = rtl match {
        case Nil => this
        case (value::lst) => BiCons(value,lst,Nil)
      }

      def getPrev = ltl match {
        case Nil => this
        case (value::lst) => BiCons(value,Nil,lst)
      }
    }

    case class BiCons[A](val hd: A, val rtl: List[A], val ltl: List[A]) extends BiIter[A] {
      def getValue = Some(hd)

      def getNext = rtl match {
        case Nil => BiNil(Nil,hd::ltl)
        case (value::lst) => BiCons(value,lst,hd::ltl)
      }

      def getPrev = ltl match {
        case Nil => BiNil(hd::rtl,Nil)
        case (value::lst) => BiCons(value,hd::rtl,lst)
      }
    }

    class ListIter[A](val rtl: List[A], val ltl: List[A]) extends BiIter[A] {
      def getValue=rtl.headOption
      def getNext = rtl match {
        case Nil => BiNil(Nil,rtl.head::ltl)
        case (_::lst) => BiCons(lst.head,lst.tail,rtl.head::ltl)
      }

      def getPrev = ltl match {
        case Nil => BiNil(rtl,Nil)
        case (value::lst) => BiCons(value,rtl.head::rtl,lst)
      }
    }

    class BiIterableList[A](val data: List[A]) extends BiIterable[A] {
      def biIter: BiIter[A] = new ListIter(data,Nil)
    }
    // null

    sealed abstract class BiIterableTree[A] extends BiIterable[A] {
      // You can write something here
      override def biIter: ListIter[A]
    }

    case class Empty[A]() extends BiIterableTree[A] {
      def biIter = new ListIter(Nil,Nil) // null
    }

    case class Node[A](value: A, left: BiIterableTree[A], right: BiIterableTree[A])
      extends BiIterableTree[A] {
      def biIter = new ListIter(value::(left.biIter.rtl ++ right.biIter.rtl),Nil) // null
    }
  }
}
