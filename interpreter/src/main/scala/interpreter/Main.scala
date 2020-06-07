package interpreter

object Main {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))

  def main(args: Array[String]): Unit = {
    print("lisp> ")
    println(Lisp.evaluate(Lisp.string2lisp(in.readLine())))
    main(args)
  }
}

object LispCode {
    // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  val reverse = """
  def (reverse L acc)
        (if (null? L) 
        acc 
        ( reverse (cdr L) (cons (car L) acc)  )
        )
  """

    // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  def (differences L)
    (def (inner list e acc)
            (if (null? list)
                acc
                (inner (cdr list) (car list) (cons (- (car list) e) acc))
             )

             (reverse (inner L 0 nil) nil)
      )
  """
  val rebuildList = """
  def (rebuildList L)
    (def (inner M)
      (if (null? (cdr M)) 
      nil 
      (cons (+ (car M) (car (cdr M))) (inner (cons (+ (car M) (car (cdr M))) (cdr (cdr M)))  )))
      (inner (cons 0 L))
    )
  """

  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
