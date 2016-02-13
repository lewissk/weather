import org.scalatest.FlatSpec

import scala.collection.mutable
import scala.util.Random

/**
  * Created by scott on 2/13/16.
  */
class WeatherSpec extends FlatSpec {

  val weather = new Weather()
  val random = Random

  info("Starting WeatherSpec Tests...")

  "containsAll" should "return true if all elements in a list exists in a second list" in {

    val list1 = List(1,2,3,4,5,6)
    val list2 = List(1,2,3,4,5,6,7)
    assert(weather.containsAll(list1, list2))

  }

  "containsAll" should "return false if all elements in a list dont exist in a second list" in {

    val list1 = List(1,3,5,7)
    val list2 = List(2,4,6,8)

    assert(weather.containsAll(list1, list2) == false)

  }

  "containsAll" should "return true if first list has no values" in {

    val list1 = List[Int]()
    val list2 = List(2,4,6,8)

    assert(weather.containsAll(list1, list2) == true)

  }

  "containsAll" should "return false if second list has no values" in {

    val list1 = List(2,4,6,8)
    val list2 = List[Int]()

    assert(weather.containsAll(list1, list2) == false)

  }

  "containsAll" should "return true for case classes" in {

    case class T(theInt: Int)

    val list1 = List(T(1), T(2))
    val list2 = List(T(1), T(2), T(3))

    assert(weather.containsAll(list1, list2))
  }

  "find5thFromLast" should "return the 5th from the last element of a linked list" in {
    val linkedList = mutable.LinkedList[Int](random.nextInt())

    for(i <- 1 to 20) {
      linkedList.append( mutable.LinkedList(random.nextInt()))
    }

    val fifthFromLast = weather.find5thFromLast(linkedList)

    assert(fifthFromLast == linkedList.get(linkedList.size - 5))
  }

  "find5thFromLast" should "return None if the list does not have sufficient number of elements in the list" in {
    val linkedList = mutable.LinkedList[Int](random.nextInt())

    for(i <- 1 to 3) {
      linkedList.append( mutable.LinkedList(random.nextInt()))
    }

    val fifthFromLast = weather.find5thFromLast(linkedList)

    assert(fifthFromLast == None)
  }

  "whatKindOfTriangleIsThis" should "return None if any side is 0" in {
    assert(weather.whatKindOfTriangleIsThis(1,1,0) == None)
  }

  "whatKindOfTriangleIsThis" should "return Equilateral when all sides are equal" in {
    assert(weather.whatKindOfTriangleIsThis(1,1,1).get == TriangleType.Equilateral)
  }

  "whatKindOfTriangleIsThis" should "return Isosceles when there are at least 2 equal sides" in {
    assert(weather.whatKindOfTriangleIsThis(1,1,3).get == TriangleType.Isosceles)
  }

  "whatKindOfTriangleIsThis" should "return Scalene when no sides are equal" in {
    assert(weather.whatKindOfTriangleIsThis(1,2,3).get == TriangleType.Scalene)
  }

}
