package pfds.ch02_persistence

import scala.List

trait Stack[F[_]]:
  def empty[T]: F[T]
  extension [T](x: F[T])
    def isEmpty: Boolean
    def cons(h: T): F[T]
    def head: T
    def tail: F[T]

given Stack[List] with
  def empty[T]: List[T] = List.empty[T]
  extension [T](x: List[T])
    def isEmpty: Boolean = x.isEmpty
    def cons(h: T): List[T] = h :: x
    def head: T = x.head
    def tail: List[T] = x.tail

enum CustomStack[+T]:
  case Cons(h: T, t: CustomStack[T])
  case Nil

given Stack[CustomStack] with
  import CustomStack.*
  def empty[T]: CustomStack[T] = Nil
  extension [T](x: CustomStack[T])
    def isEmpty: Boolean = x == Nil
    def cons(h: T): CustomStack[T] = Cons(h, x)
    private def asCons: Cons[T] = x match
      case y @ Cons(_, _) => y
      case Nil            => throw AssertionError("empty stack")
    def head: T = asCons.h
    def tail: CustomStack[T] = asCons.t