import pfds.ch02_persistence.{ given, * }

val sl = summon[Stack[List]]
val nil = sl.empty[Int]
nil.isEmpty
nil.cons(100)
nil.cons(100).cons(200)
nil.cons(100).cons(200).head
nil.cons(100).cons(200).tail

val sc = summon[Stack[CustomStack]]
val ni2 = sc.empty[Int]
ni2.isEmpty
ni2.cons(100)
ni2.cons(200).cons(100)
ni2.cons(200).cons(100).head
ni2.cons(200).cons(100).tail

// Exercise 2.1
import cats.data.NonEmptyList
def suffixes[T](l: List[T]): List[List[T]] = 
  l.foldRight(NonEmptyList.of(List.empty[T]))((a, b) => (a :: b.head) :: b).toList

suffixes(List(1,2,3,4))