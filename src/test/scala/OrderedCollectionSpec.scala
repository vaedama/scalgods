package com.github.vaedama.scalgods

import org.scalatest._

class OrderedArrayOpsSpec extends FlatSpec with Matchers {
  val emptyList = List.empty[Int]
  val ls1 = List(1, 2, 3, 4, 5, 6)
  val ls2 = List(1, 2, 3, 4, 5, 9)

  behavior of "find operation on Ordered collection"
  it should "find existing element" in {
    assert(OrderedCollection(ls1).find(4) === Some(4))
  }
  it should "not find non-existent element" in {
    assert(OrderedCollection(ls2).find(8) === None)
  }
  it should "not find element in an empty collection" in {
    assert(OrderedCollection(emptyList).find(4) === None)
  }

  behavior of "insert operation on Ordered collection"
  it should "insert existing element" in {
    assert(OrderedCollection(ls1).insert(4) === List(1, 2, 3, 4, 4, 5, 6))
  }
  it should "insert non-existent element" in {
    assert(OrderedCollection(ls2).insert(8) === List(1, 2, 3, 4, 5, 8, 9))
  }
  it should "insert in an empty collection" in {
    assert(OrderedCollection(emptyList).insert(4) === List(4))
  }

  behavior of "delete operation on Ordered collection"
  it should "delete existing element" in {
    assert(OrderedCollection(ls1).delete(4) === List(1, 2, 3, 5, 6))
  }
  it should "delete non-existent element" in {
    assert(OrderedCollection(ls2).delete(8) === List(1, 2, 3, 4, 5, 9))
  }
  it should "delete an element from an empty collection" in {
    assert(OrderedCollection(emptyList).insert(4) === List(4))
  }

  behavior of "ascended operation on Ordered collection"
  it should "work for an already ascended ordered collection" in {
    assert(OrderedCollection(ls1).ascended === ls1)
  }
  it should "work for an unordered collection (with duplicates)" in {
    assert(OrderedCollection(List(3, 5, 1, 3, 9)).ascended === List(1, 3, 3, 5, 9))
  }
  it should "work for an empty collection" in {
    assert(OrderedCollection(emptyList).ascended === emptyList)
  }

  behavior of "descended operation on Ordered collection"
  it should "work for an ascended ordered collection" in {
    assert(OrderedCollection(ls1).descended === ls1.reverse)
  }
  val ls3 = List(9, 5, 3, 3, 1)
  it should "work for an already descended ordered collection" in {
    assert(OrderedCollection(ls3).descended === ls3)
  }
  it should "work for an unordered collection" in {
    assert(OrderedCollection(List(3, 5, 1, 3, 9)).descended === ls3)
  }
  it should "work for an empty collection" in {
    assert(OrderedCollection(emptyList).ascended === emptyList)
  }
}