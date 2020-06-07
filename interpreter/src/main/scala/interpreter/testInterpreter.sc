package week12

object testInterpreter {
  import week12.Lisp._

	// How anonymous functions work

  def incrementer(x:Int) = (y:Int) => x + y       //> incrementer: (x: Int)Int => Int
  val inc = incrementer(100)                      //> inc  : Int => Int = <function1>
  inc(3)                                          //> res0: Int = 103
  inc(5)                                          //> res1: Int = 105

	// Replacing anonymous functions with objects
  
  abstract class HasApply {
  	def applyMe(arg: Int): Int
  }
  class IncrementerBody(envX: Int) extends HasApply {
  	def applyMe(argY: Int):Int = envX + argY
  }
  
  def myIncrementer(x:Int) = new IncrementerBody(x)
                                                  //> myIncrementer: (x: Int)week12.testInterpreter.IncrementerBody
  val myInc = myIncrementer(100)                  //> myInc  : week12.testInterpreter.IncrementerBody = week12.testInterpreter$$an
                                                  //| onfun$main$1$IncrementerBody$1@17f6480
  myInc.applyMe(3)                                //> res2: Int = 103
  myInc.applyMe(5)                                //> res3: Int = 105

	// Simplest interpreter

	string2lisp("(+ 41 (* 2  11))")           //> res4: week12.Lisp.Data = List('+, 41, List('*, 2, 11))
	evalExpr(string2lisp("(+ 41 (* 2 11))"))  //> res5: week12.Lisp.Data = 63

	// Interpreter with symbols
  
  string2lisp("(+ 42 (* 2 q))")                   //> res6: week12.Lisp.Data = List('+, 42, List('*, 2, 'q))
  evalSym(string2lisp("(+ 42 (* 2 q))"), Map("q" -> 10))
                                                  //> res7: week12.Lisp.Data = 62

  // Interpreter with function application

  evalFun(List('=, 31, List('*, 2, 'q)), funEnv + ("q" -> 15))
                                                  //> res8: week12.Lisp.Data = 0
  // Adding val to the interpreter

	evalVal(string2lisp("""
(val answer (+ 12 q)
		(+ answer answer)
)
"""), funEnv + ("q" -> 30))                       //> res9: week12.Lisp.Data = 84

  // Adding lambda to interpreter

	evalLambda(string2lisp("((lambda (x) (+ x x)) (* 3 z))"), funEnv + ("z" -> 15))
                                                  //> res10: week12.Lisp.Data = 90

	evalLambda(string2lisp("""
(val dup (lambda (x) (+ x x))
	(dup (dup 7))
)
"""), funEnv)                                     //> res11: week12.Lisp.Data = 28

	evalLambda(string2lisp("""
(val dup1 (lambda (x) (if (= x 10) 100 (+ x x)))
	(dup1 (dup1 10))
)
"""), funEnv)                                     //> res12: week12.Lisp.Data = 200
	
	// Lambda can do those funny things with recursion combinators
		
val zetal = string2lisp("""
(val mkZ (lambda (f)
    (val comb (lambda (x)
    		(f (lambda (v)
    			   ((x x) v)
    			  )
    		 )
    )
    (comb comb)
  )
  )
  (val factorial (lambda (fact) (lambda (x)
  (if  (= x 0) 1 (* x (fact (- x 1))))))
  ((mkZ factorial) 6) ))
""")                                              //> zetal  : week12.Lisp.Data = List('val, 'mkZ, List('lambda, List('f), List('
                                                  //| val, 'comb, List('lambda, List('x), List('f, List('lambda, List('v), List(L
                                                  //| ist('x, 'x), 'v)))), List('comb, 'comb))), List('val, 'factorial, List('lam
                                                  //| bda, List('fact), List('lambda, List('x), List('if, List('=, 'x, 0), 1, Lis
                                                  //| t('*, 'x, List('fact, List('-, 'x, 1)))))), List(List('mkZ, 'factorial), 6)
                                                  //| ))

// Believe it or not, this computed factorial of 6

evalLambda(zetal, funEnv)                         //> res13: week12.Lisp.Data = 720

// This is when we encode recursion "manually" instead of using a generic mkZ combinator

val funfact = string2lisp("""
  (val fact (lambda (n)
     ((lambda (fact)
        (fact fact n))
      (lambda (ft x)
      	(if  (= x 0) 1 (* x (ft ft (- x 1)))))
     ))
     (fact 6)
  )
 """
)                                                 //> funfact  : week12.Lisp.Data = List('val, 'fact, List('lambda, List('n), Lis
                                                  //| t(List('lambda, List('fact), List('fact, 'fact, 'n)), List('lambda, List('f
                                                  //| t, 'x), List('if, List('=, 'x, 0), 1, List('*, 'x, List('ft, 'ft, List('-, 
                                                  //| 'x, 1))))))), List('fact, 6))

 // It still works

evalLambda(funfact, funEnv)                       //> res14: week12.Lisp.Data = 720

// If we

val vactorial = string2lisp("""
(val factorial (lambda (x)
  (if  (= x 0) 1 (* x (factorial (- x 1)))))
(factorial 6) )
""")                                              //> vactorial  : week12.Lisp.Data = List('val, 'factorial, List('lambda, List('
                                                  //| x), List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial, List('-, 'x
                                                  //| , 1))))), List('factorial, 6))

// evalLambda(vactorial,  funEnv) // unknown factorial after a single unfolding


  // Finally, the avaluator that handles recursive functions
 
	val factorial = string2lisp("""
(def factorial (lambda (x)
  (if  (= x 0) 1 (* x (factorial (- x 1)))))
(factorial 3) )
""")                                              //> factorial  : week12.Lisp.Data = List('def, 'factorial, List('lambda, List('
                                                  //| x), List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial, List('-, 'x
                                                  //| , 1))))), List('factorial, 3))

	evalRec(factorial, recEnv)                //> res15: week12.Lisp.Data = 6

  // The other, usual, things work, too

	evalRec(string2lisp("""
(val dup (lambda (x) (if (= x 10) 100 (+ x x)))
	(dup (dup 10))
)
"""), recEnv)                                     //> res16: week12.Lisp.Data = 200

       
      /* This is the actual interpreter. It changes the environment once again
       	It also allows you to trace the computation. Note: the trace is rather
      	long, increase the maximum output limit to see it. */
                                                  
evaluate(factorial)                               //> ===> List('def, 'factorial, List('lambda, List('x), List('if, List('=, 'x, 
                                                  //| 0), 1, List('*, 'x, List('factorial, List('-, 'x, 1))))), List('factorial, 
                                                  //| 3))
                                                  //|  ===> List('factorial, 3)
                                                  //|   ===> 'factorial
                                                  //|    ===> List('lambda, List('x), List('if, List('=, 'x, 0), 1, List('*, 'x, 
                                                  //| List('factorial, List('-, 'x, 1)))))
                                                  //|    <=== Lambda(<function1>)
                                                  //|   <=== Lambda(<function1>)
                                                  //|   ===> 3
                                                  //|   <=== 3
                                                  //|   ===> List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial, List('-,
                                                  //|  'x, 1))))
                                                  //|    ===> List('=, 'x, 0)
                                                  //|     ===> '=
                                                  //|     <=== Lambda(<function1>)
                                                  //|     ===> 'x
                                                  //|     <=== 3
                                                  //|     ===> 0
                                                  //|     <=== 0
                                                  //|    <=== 0
                                                  //|    ===> List('*, 'x, List('factorial, List('-, 'x, 1)))
                                                  //|     ===> '*
                                                  //|     <=== Lambda(<function1>)
                                                  //|     ===> 'x
                                                  //|     <=== 3
                                                  //|     ===> List('factorial, List('-, 'x, 1))
                                                  //|      ===> 'factorial
                                                  //|       ===> List('lambda, List('x), List('if, List('=, 'x, 0), 1, List('*, '
                                                  //| x, List('factorial, List('-, 'x, 1)))))
                                                  //|       <=== Lambda(<function1>)
                                                  //|      <=== Lambda(<function1>)
                                                  //|      ===> List('-, 'x, 1)
                                                  //|       ===> '-
                                                  //|       <=== Lambda(<function1>)
                                                  //|       ===> 'x
                                                  //|       <=== 3
                                                  //|       ===> 1
                                                  //|       <=== 1
                                                  //|      <=== 2
                                                  //|      ===> List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial, List(
                                                  //| '-, 'x, 1))))
                                                  //|       ===> List('=, 'x, 0)
                                                  //|        ===> '=
                                                  //|        <=== Lambda(<function1>)
                                                  //|        ===> 'x
                                                  //|        <=== 2
                                                  //|        ===> 0
                                                  //|        <=== 0
                                                  //|       <=== 0
                                                  //|       ===> List('*, 'x, List('factorial, List('-, 'x, 1)))
                                                  //|        ===> '*
                                                  //|        <=== Lambda(<function1>)
                                                  //|        ===> 'x
                                                  //|        <=== 2
                                                  //|        ===> List('factorial, List('-, 'x, 1))
                                                  //|         ===> 'factorial
                                                  //|          ===> List('lambda, List('x), List('if, List('=, 'x, 0), 1, List('*
                                                  //| , 'x, List('factorial, List('-, 'x, 1)))))
                                                  //|          <=== Lambda(<function1>)
                                                  //|         <=== Lambda(<function1>)
                                                  //|         ===> List('-, 'x, 1)
                                                  //|          ===> '-
                                                  //|          <=== Lambda(<function1>)
                                                  //|          ===> 'x
                                                  //|          <=== 2
                                                  //|          ===> 1
                                                  //|          <=== 1
                                                  //|         <=== 1
                                                  //|         ===> List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial, Li
                                                  //| st('-, 'x, 1))))
                                                  //|          ===> List('=, 'x, 0)
                                                  //|           ===> '=
                                                  //|           <=== Lambda(<function1>)
                                                  //|           ===> 'x
                                                  //|           <=== 1
                                                  //|           ===> 0
                                                  //|           <=== 0
                                                  //|          <=== 0
                                                  //|          ===> List('*, 'x, List('factorial, List('-, 'x, 1)))
                                                  //|           ===> '*
                                                  //|           <=== Lambda(<function1>)
                                                  //|           ===> 'x
                                                  //|           <=== 1
                                                  //|           ===> List('factorial, List('-, 'x, 1))
                                                  //|            ===> 'factorial
                                                  //|             ===> List('lambda, List('x), List('if, List('=, 'x, 0), 1, List
                                                  //| ('*, 'x, List('factorial, List('-, 'x, 1)))))
                                                  //|             <=== Lambda(<function1>)
                                                  //|            <=== Lambda(<function1>)
                                                  //|            ===> List('-, 'x, 1)
                                                  //|             ===> '-
                                                  //|             <=== Lambda(<function1>)
                                                  //|             ===> 'x
                                                  //|             <=== 1
                                                  //|             ===> 1
                                                  //|             <=== 1
                                                  //|            <=== 0
                                                  //|            ===> List('if, List('=, 'x, 0), 1, List('*, 'x, List('factorial,
                                                  //|  List('-, 'x, 1))))
                                                  //|             ===> List('=, 'x, 0)
                                                  //|              ===> '=
                                                  //|              <=== Lambda(<function1>)
                                                  //|              ===> 'x
                                                  //|              <=== 0
                                                  //|              ===> 0
                                                  //|              <=== 0
                                                  //|             <=== 1
                                                  //|             ===> 1
                                                  //|             <=== 1
                                                  //|            <=== 1
                                                  //|           <=== 1
                                                  //|          <=== 1
                                                  //|         <=== 1
                                                  //|        <=== 1
                                                  //|       <=== 2
                                                  //|      <=== 2
                                                  //|     <=== 2
                                                  //|    <=== 6
                                                  //|   <=== 6
                                                  //|  <=== 6
                                                  //| <=== 6
                                                  //| res17: week12.Lisp.Data = 6


}