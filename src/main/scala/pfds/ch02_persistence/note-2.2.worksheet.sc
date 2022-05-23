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