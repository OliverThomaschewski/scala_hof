package applyHOF

import list.implementation._
import list.traits.IntList

object ProblemsLists {

  /**
    *
    * Given a number i that should be duplicated a number of times
    * returns an IntList that contains the duplicated i
    *
    * E.x. duplicateNum(4,3)
    * -> SinglyLinkedList(4, 4, 4, 4)
    *
    * @param i number to duplicate
    * @param times number of duplicates
    * @return List of duplicated numbers
    */
  def duplicateNum(i:Int, times:Int):IntList= times match{

    case 0 => Empty
    case _ => Cons(i, duplicateNum(i, times-1))
  }

  /**
    *
    * Given an IntList l that contains even and odd numbers
    * All even numbers of the list should be duplicated an number of times
    * returns an IntList that contains the all duplicated even numbers and the
    * remaining odd numbers in the same order as they occur in the origin list
    *
    * E.x. duplicateEqualNumbers(3,SinglyLinkedList(1,4,3,5,8))
    * -> SinglyLinkedList(1, 4, 4, 4, 3, 5, 8, 8, 8)
    *
    * @param times number of duplicates
    * @param l IntList that should be processed
    * @return IntList that contains the duplicates and all other nums
    */
  def duplicateEqualNumbers(times:Int, l:IntList): IntList= l match {

    case Empty => Empty
    case _ if (l.head%2 == 0) => duplicateEqualNumbers(times, l.tail).prefix(duplicateNum(l.head,times))
    case _  => Cons(l.head, duplicateEqualNumbers(times, l.tail))
  }

  /*

  Harder Problems - Optional

   */
  /**
    *
    * Given a amount of money and a list of coin values,
    * returns the number of possible ways that the change can be returned using
    * coins of those values.
    *
    * E.x. countChange(4,SinglyLinkedList(1,2)
    * has 3 solutions -> {1+1+1+1 ; 2+2 ; 1+2+1}S
    *
    * @param Money total amount of change to return
    * @param coins possible coins
    * @return number of possible ways the change can be returned
    */
def countChange (Money: Int, coins: IntList): Int = coins match{

  case Empty =>  0
  case _ =>
            Money match {

              case 0 => 1
              case _ if (Money < 0) => 0
              case _ => countChange(Money - coins.head, coins) + countChange(Money, coins.tail)
            }
  }
}