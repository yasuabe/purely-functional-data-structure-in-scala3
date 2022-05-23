package pfds.ch02_persistence

import cats.Order 

trait Set[F[_], T]:
  def empty: F[T]
  extension (x: F[T])
    def insert(e: T): F[T]
    def member(e: T): Boolean

enum Tree[+T]:
  case Node(l: Tree[T], e: T, r: Tree[T])
  case Empty 

given unbalancedSet[T](using o: Order[T]): Set[Tree, T] with
  import Tree.*, cats.Comparison.*
  def empty = Tree.Empty
  extension (tree: Tree[T])
    def member(x: T): Boolean = tree match
      case Node(l, y, r) => o.comparison(x, y) match
        case LessThan    => l.member(x)
        case GreaterThan => r.member(x)
        case EqualTo     => true
      case Empty         => false
    def insert(x: T): Tree[T] = tree match
      case Node(l, y, r) => o.comparison(x, y) match
        case LessThan    => Node(l.insert(x), y, r)
        case GreaterThan => Node(l, y, r.insert(x))
        case EqualTo     => tree
      case Empty         => Node(Empty, x, Empty)
