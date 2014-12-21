import com.fpinscala.Par
import com.fpinscala.Par.Par

object parallelism3 {
  def generalChoice[K, V](key: Par[K])(chooser: K => Par[V]): Par[V] =
    es => {
      chooser(Par.run(es)(key).get)(es)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    generalChoice(cond) { k => if (k) t else f}


  def choiceN[A](n: Par[Int])(cs: List[Par[A]]): Par[A] =
    generalChoice(n) { n => cs.drop(n).head}

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    generalChoice(key) { key => choices(key)}
}