import scala.collection.mutable

object TriangleType {
  sealed trait EnumVal
  case object Equilateral extends EnumVal
  case object Isosceles extends EnumVal
  case object Scalene extends EnumVal
  val types = Seq(Equilateral, Isosceles, Scalene)
}

object Test {

}

/**
  * Created by scott on 2/13/16.
  */
class Weather {

  /**
    * function that determines if all elements of one list are in another
    *
    * @param firstList values to check
    * @param secondList values to check against
    * @tparam A Type of the first list
    * @tparam B Type of the second list
    * @return true if all of the values exist from the firstList exist in the secondList
    */
  def containsAll[A, B <: A](firstList: List[A], secondList: List[B]): Boolean = {
    val remaining = firstList.filterNot(secondList.contains)
    remaining.size match {
      case 0 => true
      case _ => false
    }
  }

  /**
    *
    * For a single-linked (forward only) list write a function that returns nth element from the end of the list.
    * The list can only be walked once (reverse, length, or size of this list cannot be used).
    *
    * @param linkedList a linked list used to find the last digit
    * @param n the position to return from the end of the list
    * @tparam A the type of the value in the linked list
    * @return the nth element from the end of the list
    */
  def findNthFromLast[A](linkedList: mutable.LinkedList[A], n: Int): Option[A] = {
    var pointer1 = linkedList
    var pointer2 = linkedList

    for (i <- 1 to n) {

      if (pointer2 == pointer2.next) {
        return None
      } else {
        pointer2 = pointer2.next
      }
    }

    while (pointer2 != pointer2.next) {
      pointer2 = pointer2.next
      pointer1 = pointer1.next
    }
    Some(pointer1.elem)

  }

  /**
    *
    * For a single-linked (forward only) list write a function that returns 5th element from the end of the list.
    * The list can only be walked once (reverse, length, or size of this list cannot be used).
    *
    * @param linkedList a linked list used to find the last digit
    * @tparam A the type of the value in the linked list
    * @return the 5th element from the end of the list
    */
  def find5thFromLast[A](linkedList: mutable.LinkedList[A]): Option[A] = {
    findNthFromLast(linkedList, 5)
  }

  /**
    *
    * A function that takes three sides of a triangle and answers if it's equilateral, isosceles, or scalene.
    *
    * @param a Side a
    * @param b Side b
    * @param c Side c
    * @return Equilateral if all sides are the same, Isosceles if at least 2 sides are the same, and Scalene if no sides are the same
    */
  def whatKindOfTriangleIsThis(a: Int, b: Int, c: Int): Option[TriangleType.EnumVal] = {

    if(a == 0 || b == 0 || c == 0) {
      None
    } else if (a == b && a == c) {
      Some(TriangleType.Equilateral)
    } else if (a == b || b == c || a == c) {
      Some(TriangleType.Isosceles)
    } else {
      Some(TriangleType.Scalene)
    }

  }

}



