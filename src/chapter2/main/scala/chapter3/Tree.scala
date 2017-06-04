package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	def apply() = {

	}

	// 3.25
	def size[A](t: Tree[A]): Int = t match {
		case Leaf(v) => 1
		case Branch(l, r) => 1 + size(l) + size(r)
	}

	// 3.26
	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(v) => v
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	// 3.27
	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(v) => 1
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	// 3.28
	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	// 3.29
	def fold[A, B](t: Tree[A])(fl: A => B)(fb: (B, B) => B): B = t match {
		case Leaf(v) => fl(v)
		case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
	}

	def sizeByFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

	def maximumByFold(t: Tree[Int]): Int = fold(t)((x: Int) => x)((x: Int, y: Int) => x max y)

	def depthByFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ max _)

	def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
		def leaf(a: A): Tree[B] = Leaf(f(a))
		def branch(b1: Tree[B], b2: Tree[B]): Tree[B] = Branch(b1, b2)
		fold(t)(leaf)(branch)
	}
}

object treeTest extends App {
	val tree1 = Leaf(1)
	val tree2: Tree[Int] = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(6)), Branch(Leaf(3),Leaf(4)))

	println(tree1)
	println(tree2)
	println(Tree.size(tree2))
	println(Tree.sizeByFold(tree2))
	println(Tree.maximum(tree2))
	println(Tree.maximumByFold(tree2))
	println(Tree.depth(tree2))
	println(Tree.depthByFold(tree2))

	println(Tree.map(tree2)(_ + 1))
	println(Tree.mapByFold(tree2)(_ + 1))
}
