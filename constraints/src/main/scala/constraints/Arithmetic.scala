package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._
import Math._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its integer representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = n.foldRight((0,0)){case (b,(acc,exp)) =>
    if(b) (acc + Math.pow(2,exp).toInt , exp+1)
    else (acc , exp+1)
  }._1

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = {
    @tailrec
    def inner(i: Int, acc: List[Boolean]): List[Boolean] = {
      if (i == 0) acc
      else if (i % 2 == 0) inner(i / 2, false :: acc)
      else inner((i - 1) / 2, true :: acc)
    }

    if (n == 0) List(false)
    else inner(n, List())

  }


  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    @tailrec //Defines strictly greater
    def inner(l: List[(Formula, Formula)], form: Formula, acc: Formula): Formula = {
      if (l.isEmpty) acc
      else l.head match {
        case (x, y) => inner(l.tail, form && (x iff y), acc || and(x, !y, form))
      }
    }

    val t : Formula = false;
    !inner(n1.reverse.zipAll(n2.reverse, t, t).reverse, true, false)
  }

  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = (or(and(a, b), and(cIn, !(a iff b))), !(a iff !(b iff cIn)))

  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    def inner(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
      val (a,b) = (propVar(), propVar())
      (n1, n2) match {
        case (x :: Nil, y :: Nil) => {
          val (head, last) = fullAdder(x, y, false)
          (a :: b :: Nil, Set(a iff head, b iff last))
        }
        case (x :: xs, y :: ys) => {
          val (z :: zs, constraint) = inner(xs, ys)
          val (head, second) = fullAdder(x, y, z)
          (a :: b :: zs, constraint + (a iff head, b iff second))
        }
        case _ => sys.error("Unexpected case")
      }
    }

    val (x, y) = uniformize(n1, n2)
    inner(x, y)
  }

  def uniformize(n1: List[Formula], n2: List[Formula]): (List[Formula], List[Formula]) = {
    @tailrec
    def inner(size: Int, acc: List[Formula]): List[Formula] = {
      if (size <= 0) acc
      else inner(size - 1, false :: acc)
    }

    (n1.size - n2.size) match {
      case 0 => (n1, n2)
      case x => if (x < 0) (inner(-x, n1), n2) else (n1, inner(x, n2))
    }
  }

  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }


}
