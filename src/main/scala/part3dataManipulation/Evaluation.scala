package part3dataManipulation

object Evaluation extends App {

  /*
  Cats evaluation
  - eagerly
  - lazily
  = lazy with memoization
   */

  import cats.Eval

  val instantEval = Eval.now {
    println("Compute Now")
    42
  }

  val redoEval = Eval.always {
    println("Compute Again")
    4
  }

  //println(redoEval.value)
  //println(redoEval.value)

  val delayedEval = Eval.later {
    println("Compute Later")
    5
  }
  //println(delayedEval.value)
  //println(delayedEval.value)

  val composedEvaluation = instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))

  // println(composedEvaluation.value)
  //  println(composedEvaluation.value)

  val anotherComposedEval = for {
    v1 <- instantEval
    v2 <- delayedEval
  } yield v1 + v2

  val dontRecompute = redoEval.memoize

  // println(dontRecompute.value)
  // println(dontRecompute.value)

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  val deferred = defer(Eval.now {
    println("Now")
    42
  })
  println(deferred.value)

  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  def reverseEval[T](list: List[T]): Eval[List[T]] = list match {
    case Nil => Eval.now(Nil)
    case head :: tail => defer(reverseEval(tail).map(_ :+ head))
  }

  // chains defer calls are evaluated tail recursively

  println(reverseEval((1 to 10000).toList).value)
}
