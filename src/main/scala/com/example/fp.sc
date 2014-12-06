import com.example.{ListHOFs, BasicExercises}

object fp {
  BasicExercises.fib(3)
  BasicExercises.fib(4)
  BasicExercises.fib(5)
  BasicExercises.fib(6)
  BasicExercises.factorial(5)
  BasicExercises.indexOf[Int](Array(1,2,3), {
    case 2 => true
    case _ => false
  })

  BasicExercises.isSorted[Int](Array(1,2,3), {_ < _})
  BasicExercises.isSorted[Int](Array(1,2,3), {_ > _})

  ListHOFs.product(List(2,2,3))
}
