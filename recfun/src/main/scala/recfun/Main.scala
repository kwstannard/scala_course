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
    if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r) + pascal(c, r - 1)


  def foo: Int = {
    var foo = 1
    foo = 2
    foo
  }

  /**
   * Exercise 2
   */
  def balance(chars: => List[Char]): Boolean =
    balanceHelper(chars, 0)

  def balanceHelper(chars: List[Char], tally: Int): Boolean =
    if(tally < 0)
      false
    else if (chars.isEmpty)
      tally == 0
    else if (chars.head == "(".head)
      balanceHelper(chars.tail, tally + 1)
    else if (chars.head == ")".head)
      balanceHelper(chars.tail, tally - 1)
    else
      balanceHelper(chars.tail, tally)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty)
      0
    else if(money == 0)
      1
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
}

