import java.util.concurrent.{Executors, Future}

import com.fp.Par
import com.fp.Par.Par

object parallelism3 {
  val pool = Executors.newSingleThreadExecutor()

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    generalChoice(cond) { k => if (k) t else f}

  def generalChoice[K, V](key: Par[K])(chooser: K => Par[V]): Par[V] =
    es => {
      chooser(Par.run(es)(key).get)(es)
    }

  def choiceN[A](n: Par[Int])(cs: List[Par[A]]): Par[A] =
    generalChoice(n) { n => cs.drop(n).head}

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    generalChoice(key) { key => choices(key)}

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(Par.map(a)(f))

  // flatMap consists of 2 steps - mapping and flattening.
  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val run: Future[Par[A]] = Par.run(es)(a)
      run.get()(es)
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a) { a => a}

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      flatMap(a) { a =>
        flatMap(b) { b =>
          Par.unit(f(a, b))
        }
      }(es)
    }

  // generalChoice is actually...
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    generalChoice(a)(f)

  Par.run(pool)(flatMapViaJoin(Par.unit(1))(x => Par.unit(x + 1))).get
  Par.run(pool)(joinViaFlatMap(Par.unit(Par.unit(1)))).get
  Par.run(pool)(map2(Par.unit(1), Par.unit(2))(_ + _)).get

  // The core idea of all parallelism exercises is to reduce a pure functional
  // API to a minimal set of primitive functions.
  // Good explanations and derived functions are required.
}