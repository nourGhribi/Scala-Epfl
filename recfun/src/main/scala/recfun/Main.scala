package recfun

object Main {
  def main(args: Array[String]): Unit = {
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
      if (c == 0 || r == c) 1
      else pascal(c,r-1)+pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean ={
      def count(chars: List[Char], sum: Int): Boolean={
        if(sum<0) false
        else if(chars.isEmpty && sum==0) true
        else if(chars.isEmpty && sum!=0) false
        else if(chars.head=='(') count(chars.tail, sum+1)
        else if(chars.head==')') count(chars.tail, sum-1)
        else count(chars.tail, sum)
      }
      count(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0) 1
      else if(coins.isEmpty || money<0) 0
      else countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
