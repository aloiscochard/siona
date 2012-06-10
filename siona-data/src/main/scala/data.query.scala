//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data
package query

sealed trait Predicate[T, A[T]] {
  val source: A[T]
}
case class Equal[T, A[T]](override val source: A[T], value: T) extends Predicate[T, A]
case class Greater[T, A[T]](override val source: A[T], value: T) extends Predicate[T, A]

case class Predictable[T, A[T]](source: A[T]) {
  //  >     greaterThan
  //  <     lessThan
  //
  //  %==   startsWith
  //  ==%   endsWith
  //  =%=   contains
  //
  //  ===   equalTo
  //  =!=   notEqualTo
  def >(x: T) = Greater(source, x)

  def ===(x: T) = Equal(source, x)
}
