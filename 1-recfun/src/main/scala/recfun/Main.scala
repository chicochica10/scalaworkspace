package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   * There are three methods on List[Char] that are useful for this exercise:
   *
   * chars.isEmpty: Boolean returns whether a list is empty
   * chars.head: Char returns the first element of the list
   * chars.tail: List[Char] returns the list without the first element
   * Hint: you can define an inner function if you need to pass extra parameters to your function.
   *
   * Testing: You can use the toList method to convert from a String to a List[Char]: e.g. "(just an) example".toList.
   */
  def balance(chars: List[Char]): Boolean = {

    def countRest(count: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) count == 0
      else if (count == 0)
        if (chars.head == ')') false
        else if (chars.head == ')') countRest(count - 1, chars.tail)
        else if (chars.head == '(') countRest(count + 1, chars.tail)
        else countRest(count, chars.tail)
      else if (chars.head == ')') countRest(count - 1, chars.tail)
      else if (chars.head == '(') countRest(count + 1, chars.tail)
      else countRest(count, chars.tail)

    countRest(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

     if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail);
  }

}
