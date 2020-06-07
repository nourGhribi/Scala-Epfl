/**
  * We represent a set by its characteristic function, i.e.
  * its `contains` predicate.
  */
type Set = Int => Boolean

/**
  * Indicates whether a set contains a given element.
  */
def contains(s: Set, elem: Int): Boolean = s(elem)

/**
  * Returns the set of the one given element.
  */
def singletonSet(elem: Int): Set = x => x == elem

contains(singletonSet(5),5)

contains(singletonSet(5),7)


/**
  * Returns the union of the two given sets,
  * the sets of all elements that are in either `s` or `t`.
  */
def union(s: Set, t: Set): Set = x => contains(s,x) || contains(t,x)

singletonSet(5)
singletonSet(7)

contains(union(singletonSet(5),singletonSet(7)),5)
contains(union(singletonSet(5),singletonSet(7)),7)
contains(union(singletonSet(5),singletonSet(7)),12)


/**
  * Returns the intersection of the two given sets,
  * the set of all elements that are both in `s` and `t`.
  */
def intersect(s: Set, t: Set): Set = x => contains(s,x) && contains(t,x)

val set7 = singletonSet(7)

val set5 = singletonSet(5)

val set5_7 = union(set5,set7)

val set12 = singletonSet(12)

val set5_7_12 = union(set5_7,set12)
contains(intersect(set5,set5_7),5)
contains(intersect(set5,set5_7),7)
contains(intersect(set12,set5_7),7)
contains(intersect(set5_7_12,set12),12)
contains(intersect(set5_7_12,set12),5)

/**
  * Returns the difference of the two given sets,
  * the set of all elements of `s` that are not in `t`.
  */
def diff(s: Set, t: Set): Set = x => contains(s,x) && !contains(t,x)

val setNeg:Set = x => (x < 0)

val setAbs3:Set = x => (x >= -3) && (x <= 3)

val setDiff = diff(setNeg,setAbs3)

contains(setDiff,3)

contains(setDiff,5)

contains(setDiff,12)

contains(setDiff,-12)

/**
  * Returns the subset of `s` for which `p` holds.
  */
def filter(s: Set, p: Int => Boolean): Set = x => contains(s,x) && contains(p,x)

contains(filter(setNeg,setAbs3),-5)

contains(filter(setNeg,setAbs3),-3)

contains(filter(setNeg,setAbs3),-2)

contains(filter(setNeg,setAbs3),0)

/**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s,a) && !p(a)) false
      else iter(a+1)
    }
    iter(- bound)
  }

forall(setNeg,setAbs3)

forall(setNeg, x => x < 0)

forall(setNeg, x => x < 5)

forall(setNeg, x => x < -5)

forall(setAbs3, x => (x >= -2) && (x <= 2))

forall(x => (x >= -2) && (x <= 2),setAbs3)

forall(x => ( x <= 0 ) && (x % 2 == 0),x => x%2==0)

forall(x => x%2==0,x => ( x <= 0 ) && (x % 2 == 0))

/**
  * Returns whether there exists a bounded integer within `s`
  * that satisfies `p`.
  */
def exists(s: Set, p: Int => Boolean): Boolean = forall(intersect(s,p),s)

exists(setNeg,setAbs3)

exists(set5,set5_7)

exists(set7,set12)