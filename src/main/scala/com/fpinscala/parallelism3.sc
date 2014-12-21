import com.fpinscala.Par
import com.fpinscala.Par.Par

object parallelism3 {
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(Par.map(cond) { r =>
      if (r) 1
      else 0
    })(List(f, t))

  def choiceN[A](n: Par[Int])(cs: List[Par[A]]): Par[A] =
    es => {
      cs.drop(Par.run(es)(n).get).head(es)
    }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      choices(Par.run(es)(key).get)(es)
    }

  def generalChoice[K, V](key: Par[K])(chooser: K => Par[V]): Par[V] =
    es => {
      chooser(Par.run(es)(key).get)(es)
    }
}