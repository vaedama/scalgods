package com.github.vaedama.scalgods

import scala.annotation.tailrec

case class OrderedCollection[A <% Ordered[A]](ls: List[A]) {

  /**
   * If we perform Binary Search on an ordered list of elements,
   * the time complexity should not be more than O(log N).
   * Linear Search: Worst = O(N), Best = O(1), Average = O(N)
   * Binary Search: Worst = O(log N), Best = O(1), Average = O(log N)
   */
  def find(element: A): Option[A] = {
    @tailrec
    def step(mid: Int, ls: List[A]): Option[A] =
      ls match {
        case Nil => None
        case head :: Nil => if (head == element) Some(head) else None
        case xs =>
          val (lows, highs) = xs.splitAt(mid)
          if (xs(mid) > element) step(lows.size / 2, lows)
          else step(highs.size / 2, highs)
      }
    step(ls.size / 2, ls)
  }

  /**
   * This operation even works if the element to be inserted already exists in the
   * collection or even if the collection is empty.
   * Insertion in an ordered array of elements takes O(N) as we have to shift all cells one step back
   * Insertion in an unordered array of elements takes O(1) as we have do not need to shift any cells
   */
  def insert(element: A): List[A] = {
    val (lows, highs) = ls.splitAt(ls.indexWhere(_ >= element))
    lows ::: element :: highs
  }

  /**
   * Deleting an element in ordered or unordered array is always linear time because we have
   * to find the element we want to delete and shift the cells one step towards head.
   * Hence the complexity in both the cases is roughly O(N)
   */
  def delete(element: A) =
    ls.filterNot(_ == element)

  /**
   * Scala collections have a sorted pimp defined on them which sorts in ascending order by default
   */
  def ascended =
    ls.sorted

  /**
   * We can definitely do better than this by creating implicit objects for each of the subtypes of AnyVal:
   * Int, Float, Double, String, Char, Boolean, Byte and others extending Ordering trait and overriding the
   * compare method to define a reverse sorting by using minus (-) operator
   */
  def descended =
    ls.sorted.reverse
}
