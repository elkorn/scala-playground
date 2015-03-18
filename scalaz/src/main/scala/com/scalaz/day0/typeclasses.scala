package com.scalaz.day0;

// Example from http://debasishg.blogspot.com/2010/06/scala-implicits-type-classes-here-i.html
object AdaptersAndTypeclassesExample {
  case class Address(
    number: Int, 
    street: String, 
    city: String, 
    state: String, 
    zip: String) 

  import annotation.implicitNotFound
  @implicitNotFound("No member of type class LabelMaker in scope for ${T}")
  trait LabelMaker[T] {
   def toLabel(t: T): String  
  }

  object Adapters {
    object AddressLabelMaker extends LabelMaker[Address] {
      def toLabel(address: Address): String = {
        import address._
        "%d %s, %s, %s - %s".format(
          number, 
          street, 
          city, 
          state, 
          zip) 
      }
    }

    // The problem with the OO approach is that adapters do not compose well.
    // See that we are introducint incidental complexity by hiding the identity of
    // the Address within the AddressLabelMaker.
    def show() = print(AddressLabelMaker.toLabel(Address(0, "", "", "", "")))
  }

  object Typeclasses {
    // Using the typeclass approach makes the program select the correct adapter
    // instance during compile time, so that the user can operate on the adaptee
    // directly.

    object LabelMaker {
      implicit object AddressLabelMaker extends  LabelMaker[Address] {
        def toLabel(address: Address): String = {
          import address._
          "%d %s, %s, %s - %s".format(
            number, 
            street, 
            city, 
            state, 
            zip) 
        }
      }
    }

    import LabelMaker._

    // Marking the `lm` as implicit makes the compiler look for a value to 
    // supply for it within the lexical scope. This value must also be marked as
    // implicit.
    def printLabel[T](t: T)(implicit lm: LabelMaker[T]): String = lm.toLabel(t)

    // Context-bound syntax allows to use a 'placeholder' instead of having to
    // specify the `lm` parameter within function declaration.
    def printLabelContextBound[T: LabelMaker](t: T) = implicitly[LabelMaker[T]].toLabel(t)

    def show() = print(printLabel(Address(0, "", "", "", "")))
    // The compiler catches that we support only the Address type.
    // def showInt() = print(printLabel(12))

  }
}
