import com.example.FPExercises
object fp {
  FPExercises.fib(3)
  FPExercises.fib(4)
  FPExercises.fib(5)
  FPExercises.fib(6)
  FPExercises.factorial(5)
  FPExercises.indexOf[Int](Array(1,2,3), {
    case 2 => true
    case _ => false
  })

  FPExercises.isSorted[Int](Array(1,2,3), {_ < _})
  FPExercises.isSorted[Int](Array(1,2,3), {_ > _})
}
