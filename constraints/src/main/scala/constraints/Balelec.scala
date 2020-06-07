package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
 * This component implements a constraint solver for assigning time slots to volunteers
 * for various tasks at a festival. A task may require more than one volunteer,
 * and a volunteer can take a limited number of tasks
 */
object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
   * A task is represented by its name and
   * its capacity, i.e. the exact number of people
   * required to complete it.
   */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
   * This function schedules volunteers to tasks.
   * It takes as input a list of volunteers and a list of tasks.
   * The `availability` map contains mappings from volunteers to the
   * tasks they are available for.
   * A volunteer can be assigned to several tasks, but only
   * up to a maximum number of task specified by the `maxWorkload` parameter.
   * It is ok to not assign a volunteer to any task.
   *
   * The return value is a list of volunteers assigned to each task. The function only
   * returns a complete valid assignment, if no such assignment exists then the
   * function returns None.
   */
  def schedule(
    volunteers: List[Volunteer],
    tasks: List[Task],
    availability: Map[Volunteer, List[Task]],
    maxWorkload: Int
  ): Option[Map[Task, List[Volunteer]]] = {

    val variables: Map[(Volunteer, Task), PropVar] =
      volunteers.flatMap({
        case v@Volunteer(name) =>
          tasks.map(t => (v, t) -> propVar(name + "_" + t.name))
      }).toMap

    val notOverloaded: Seq[Formula] =
      (volunteers map (v => atMostMaxTrue(tasks map (t => variables(v, t)), maxWorkload))).toSeq

    val taskDesiredOnly: Seq[Formula] = {
      volunteers map (v => {
        val l: List[Task] = availability(v)
        (for {
          t <- tasks
          if !(l.contains(t))
        } yield !variables(v, t)).foldLeft[Formula](true)((acc, el) => acc && el)
      })
    }.toSeq

    val correctNumbers: Seq[Formula] = (tasks map (t => equals(volunteers map (variables(_, t)), t.capacity))).toSeq

    val allConstraints: Seq[Formula] =
      notOverloaded ++ taskDesiredOnly ++ correctNumbers

    val s = solveForSatisfiability(and(allConstraints: _*))

    s.map(m => tasks.map(t => (t, volunteers.filter(v => m(variables((v, t)))))).toMap)
  }

  def equals(ns: List[Formula], n: Int): Formula = {
    val (r, c) = countPositiveBits(ns)
    lessEquals(r, int2binary(n)) && lessEquals(int2binary(n), r) && and(c.toSeq: _*)
  }

  /**
   * This function takes a list of constraint, and returns a
   * constraint that is true if and only if at most max
   * of them are true.
   */
  def atMostMaxTrue(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositiveBits(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  }

  /**
   * This function counts the number of positive bits in a number.
   * 
   * It takes a list of formulas, and returns a pair.
   * The first element of the pair is a list of formulas representing the number
   * of ones in `ns`.
   * The second element is a set of additional constraints that have been gathered along
   * the way. Hint: see `adder` for understanding how to use additional constraints
   */
  def countPositiveBits(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    ns.foldLeft((List[Formula](false), Set[Formula]())) { case ((tmpSum, tmpAcc), n) =>
      val (r, c) = adder(tmpSum, List(n))
      (r, tmpAcc ++ c)
    }
  }

}
