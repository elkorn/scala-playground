object Main extends App {

  object problem {
    // The problem here is that the function's main concern is
    // boolean negation and we're mixing in the concern of 
    // aggregating the logger string.
    def negate(b: Boolean, logger: String): Tuple2[Boolean, String] =
      return (!b, logger + "Not so! ")

    // This is a bit better, we are not expecting any log to be
    // aggregated, only the core responsibility remains along
    // with the 'information' strictly related to it (to be
    // appended to the log by some external entity).
    def negate(b: Boolean): Tuple2[Boolean, String] =
      return (!b, "Not so! ")
  }

  // This category allows piggybacking log strings onto the actual
  // return type.
  type Writer[A] = Tuple2[A, String]
  private def toUpper(s: String): String = s.toUpperCase
  private def words(s: String): Array[String] = s.split(' ')
  private def toWords(s: String): Array[String] = words(s)

  object embellished {
    def toUpper(s: String): Writer[String] = (Main.toUpper(s), "toUpper ")
    def toWords(s: String): Writer[Array[String]] = (Main.toWords(s), "toWords ")
    def negate(b: Boolean): Writer[Boolean] =
      (!b, "negate ")

    // This is the external function that takes care of log 
    // aggregation.
    // This pattern will lead to a lot of code duplication, since 
    // different utility functions will probably have to handle 
    // log aggregation as well.
    // This has to be abstracted - but in this case we actually
    // have to abstract function composition itself.
    def process(s: String): Writer[Array[String]] = {
      val p1 = toUpper(s)
      val p2 = toWords(p1._1)
      return (p2._1, p1._2 + p2._2)
    }
  }

  object morphisms {
    private def isEvenCore(n: Int): Boolean = n % 2 == 0

    // Transform the core function to a morphism represented by
    // the embellished function.
    // This is still an Int->Boolean morphism as well.
    def isEven(n: Int): Writer[Boolean] = 
      (isEvenCore(n), "isEven ")

    // The isEven morphism should be composable with any 
    // Boolean->* morphism.
    def isOdd(n: Int): Writer[Boolean] ={
      val p1 = isEven(n)
      val p2 = embellished.negate(p1._1)

      (p2._1, p1._2+p2._2)
    }
  }

  object kleisli {
    // After writing isOdd, there are 2 examples of composition and
    // the rules become clear. They can be materialized as follows:
    def compose[A,B,C](m1: A=>Writer[B], m2: B=>Writer[C]):A=>Writer[C]= 
      (a: A) => {
        val p1 = m1(a)
        val p2 = m2(p1._1)

        (p2._1, p1._2 + p2._2)
    }

    // The last thing that's needed for the writer to be a legit
    // category is an identity morphism.
    def identity[A](a: A): Writer[A] = (a, "")

    // These are not part of the category. Or are they?
    // I need to get the terminology and boundaries straight.
    val isOdd: Int => Writer[Boolean] = 
      compose(morphisms.isEven, embellished.negate)
    val process: String => Writer[Array[String]] = 
      compose(embellished.toUpper, embellished.toWords)
  }

  trait Monoid[A] {
    def mappend(a1: A, a2: => A): A
    def mzero: A
  }

  object StringMonoid extends Monoid[String] {
    def mappend(a1:String, a2: => String) = a1 + a2
    def mzero = ""
  }

  object generalMonoidKleisli {
    type Writer[A, B <: Monoid[C]] = Tuple2[A, B] 

    // type StringWriter[A] = Writer[String, StringMonoid]
    // I don't know yet how to achieve this.

    // The composition now will be based on mappend and mzero
    // instead of `+` and `""`.
    // def compose[A,B,C,]


  }

  println(embellished.toWords("abra cadabra"))
  println(embellished.process("abra cadabra"))
  println(morphisms.isEven(2))
  println(morphisms.isOdd(2))
  println(kleisli.isOdd(2))
  println(kleisli.process("abra cadabra"))
}

