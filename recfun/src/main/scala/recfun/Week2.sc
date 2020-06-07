
/** tail recursion **/
def factorial(n:Int):Int = {
  def loop(n:Int,acc:Int):Int =
    if(n == 0) acc
    else loop(n-1,acc*n)

  loop(n,1)
}
factorial(3)
factorial(5)

def puissance(base:Int,exp:Int):Int = {
  def loop(base: Int, exp: Int, acc: Int): Int =
    if (exp == 0) acc
    else if (exp % 2 == 0) loop(base, exp / 2, acc * base * base)
    else loop(base, exp - 1, base * acc)

  loop(base, exp, 1)
}
puissance(2,3)
puissance(2,2)

/** higher order functions **/
def sum(f: Int=>Int,a:Int,b:Int): Int = {

  def loop(a:Int,acc:Int):Int =
    if(a > b) acc
    else loop(a+1,f(a)+acc)

 loop(a,0)
}
sum(x => x*x,2,4)
sum(x => x,2,4)
sum(factorial,2,3)

/** currying **/
def product(f: Int => Int)(a:Int,b:Int):Int =
 if(a > b ) 1 else f(a)*product(f)(a+1,b)

product(x => x)(2,4)

def sumProd = product(x => x)(_,_)

sumProd(2,4)

def fact(n : Int) = product(x => x )(1,n)

fact(5)
fact(3)

def mapReduce(unit:Int,g: (Int,Int) =>Int)(f: Int => Int)(a:Int,b:Int):Int =
if(a > b) unit else g(f(a),mapReduce(unit,g)(f)(a+1,b))

def somme = mapReduce(0,(x,y) => x+y)(x=>x)(_,_)

somme(1,3)

def produit = mapReduce(1,(x,y) => x*y)(x=>x)(_,_)

produit(1,3)



