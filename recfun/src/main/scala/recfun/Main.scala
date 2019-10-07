package recfun

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
    def pascal(c: Int, r: Int): Int = {
      (c, r) match {
        case (0,1) => 1
        case _ => pascal(c, r - 1) + pascal(c - 1, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(
        chars: List[Char],
        opening: Int = 0,
        closing: Int = 0,
        parens: List[Option[Char]] = List.empty
      ): Boolean = {
        chars match {
          case Nil => {
            if (
              opening == closing &&
              (parens.head match {
                case Some('(') | None => parens.last match {
                  case Some(')') | None => true
                  case _ => false
                }
                case _ => false
              })
            )  
            true else false
          }
          case h :: tail => h match {
            case '(' => balance(tail, opening + 1, closing, parens :+ Some('('))
            case ')' => balance(tail, opening, closing + 1, parens :+ Some(')'))
            case _   => balance(tail, opening, closing, parens)
          }
        }
      }
      balance(chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
