import cats.Order
import pfds.ch02_persistence.{ given, * }

val t0: Tree[String] = Tree.Empty
t0.member("x")

val c = t0.insert("c")
c == c.insert("c")
c.member("c")

val ac = c.insert("a")
ac.member("a")
ac.member("b")
ac.member("c")

val abc = ac.insert("b")
val abcd = abc.insert("d")
val abbc = abc.insert("b1")

"abcd".forall(ch => abcd.member(s"$ch"))

enum Lvl:
  case L, M, H

import Lvl.*
given Order[Lvl] with
  def compare(l1: Lvl, l2: Lvl) = summon[Order[Int]].compare(l1.ordinal, l2.ordinal)

val l0: Tree[Lvl] = Tree.Empty
val l   = l0.insert(L)
val lh  = l.insert(H)
val lmh = lh.insert(M)
