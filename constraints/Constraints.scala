package week10

object Constraints {
  def CFconverter(c: Quantity, f: Quantity) = {
    val u, v, w, x, y = new Quantity
    Constant(w, 9); Multiplier(c, w, u)
    Constant(y, 32); Adder(v, y, f)
    Constant(x, 5); Multiplier(v, x, u)
  }

  def main(args : Array[String]) : Unit = {
    val C, F = new Quantity
    CFconverter(C, F)
    Probe("Celsius temp", C)
    Probe("Fahrenheit temp", F)
    C setValue 25
//    C.forgetValue
    F setValue 212
  }
}

abstract class Constraint {
  def newValue: Unit
  def dropValue: Unit
}

case class Adder(a1: Quantity, a2: Quantity, sum: Quantity)
           extends Constraint {
  def newValue = (a1.getValue, a2.getValue, sum.getValue) match {
    case (Some(x1), Some(x2), _) => sum.setValue(x1 + x2, this)
    case (Some(x1), _, Some(r))  => a2.setValue(r - x1, this)
    case (_, Some(x2), Some(r))  => a1.setValue(r - x2, this)
    case _ =>
  }
  def dropValue {
    a1.forgetValue(this); a2.forgetValue(this); sum.forgetValue(this)
  }
  a1 connect this
  a2 connect this
  sum connect this
}

case class Multiplier(a1: Quantity, a2: Quantity, prod: Quantity)
           extends Constraint {
  def newValue = (a1.getValue, a2.getValue, prod.getValue) match {
    case (Some(x1), Some(x2), _) => prod.setValue(x1 * x2, this)
    case (Some(x1), _, Some(r))  => a2.setValue(r / x1, this)
    case (_, Some(x2), Some(r))  => a1.setValue(r / x2, this)
    case _ =>
  }
  def dropValue {
    a1.forgetValue(this); a2.forgetValue(this); prod.forgetValue(this)
  }
  a1 connect this
  a2 connect this
  prod connect this
}

case class Constant(q: Quantity, v: Double) extends Constraint {
  def newValue: Unit = error("Constant.newValue")
  def dropValue: Unit = error("Constant.dropValue")
  q connect this
  q.setValue(v, this)
}

class Quantity {
  private var value: Option[Double] = None
  private var constraints: List[Constraint] = List()
  private var informant: Constraint = NoConstraint;

  def getValue: Option[Double] = value

  def setValue(v: Double, setter: Constraint) = value match {
    case Some(v1) =>
      if (v != v1) error("Error! contradiction: " + v + " and " + v1)
    case None =>
      informant = setter; value = Some(v)
      for (c <- constraints if c != informant) c.newValue
  }
  def setValue(v: Double): Unit = setValue(v, NoConstraint)

  def forgetValue(retractor: Constraint) {
    if (retractor == informant) {
      value = None
      for (c <- constraints if c != informant) c.dropValue
    }
  }
  def forgetValue: Unit = forgetValue(NoConstraint)

  def connect(c: Constraint) {
    constraints = c :: constraints
    value match {
      case Some(_) => c.newValue
      case None =>
    }
  }
}

case class Probe(name: String, q: Quantity) extends Constraint {
  def newValue: Unit = printProbe(q.getValue)
  def dropValue: Unit = printProbe(None)
  private def printProbe(v: Option[Double]) {
    val vstr = v match {
      case Some(x) => x.toString()
      case None => "?"
    }
    println("Probe: " + name + " = " + vstr)
  }
  q connect this
}

object NoConstraint extends Constraint {
  def dropValue: Unit = ???
  def newValue: Unit = ???
}
