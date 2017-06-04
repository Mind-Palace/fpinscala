package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	// 3.2
	def tail[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(_, xs) => xs
	}

	// 3.3
	def setHead[A](as: List[A], value: A): List[A] = as match {
		case Nil => Cons(value, Nil)
		case Cons(x, xs) => Cons(value, xs)
	}

	// 3.4
	def drop[A](l: List[A], n: Int): List[A] =
		if (n > 0)
			l match {
				case Nil => Nil
				case Cons(x, xs) => drop(xs, n - 1)
			}
		else l

	// 3.5
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
	}

	// 3.6
	// tail처럼 상수시간으로 구현할 수 없다. 왜 그럴까?
	// => 앞에 것만 따로 복제가 필요해서 그런 듯.
	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(x, Nil) => Nil
		case Cons(x, xs) => Cons(x, init(xs))
	}


	//
	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

	def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

	// 3.9
	def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

	// 3.10
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	// 3.11
	def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

	def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

	// 3.12
	def reverse[A](ns: List[A]) = foldLeft(ns, Nil: List[A])((y, x) => Cons(x, y))

	// 3.13 ****
//	def foldLeftByRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight()
	def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
		foldLeft(reverse(l), z)((b,a) => f(a,b))

	def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
		foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

	def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
		foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

	// 3.14
	def append[A](ns: List[A], v: A): List[A] = foldRight(ns, List(v))(Cons(_,_))

	// 3.15
	def append[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)(Cons(_, _))


	// 3.16
	def addOne(ns: List[Int]): List[Int] = foldRight(ns, Nil: List[Int])((x, y) => Cons(x+1, y))

	// 3.17
	def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((d, s) => Cons(d.toString, s))//Cons(h.toString, doubleToString(t))

	// 3.18
	def map[A,B](as: List[A])(f: A => B): List[B] = as match {
			case Nil => Nil
			case Cons(h, t) => Cons(f(h), map(t)(f))
	}


	// 3.19
	def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
		case Nil => Nil
		case Cons(h, t) if !f(h) => Cons(h, filter(t)(f))
		case Cons(h, t) => filter(t)(f)
	}

	// 3.20
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
		case Nil => Nil
		case Cons(h, t) => List.append(f(h), flatMap(t)(f))
	}

	// 3.21
//	def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
//		def f2(a: A): List[A] =
//			if f(a) Nil: List[A]
//			else a
//	}


	// 3.22
	def sumList(as: List[Int], bs: List[Int]): List[Int] = {
		def loop(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
				case (Nil, Nil) => Nil
				case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumList(t1, t2))
		}
		if (length(as) == length(bs)) loop(as, bs)
		else Nil // error
	}

	// 3.23
	def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
		def loop(as: List[A], bs: List[B]): List[C] = (as, bs) match {
			case (Nil, Nil) => Nil: List[C]
			case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), loop(t1, t2))
		}

		if (length(as) == length(bs)) loop(as, bs)
		else Nil // error
	}

	// 3.24
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
		def matchLoop(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
			case (_, Nil) => true // sublist가 끝에 도달 => subsequence
			case (Nil, _) => false // sublist가 끝에 도달하기 전에 sup가 먼저 도달 => not subsequnce
			case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => matchLoop(t1, t2) // match start from this
			case (Cons(h1, t1), Cons(h2, t2)) if (h1 != h2) => false // match start from next element
		}

		(sup, sub) match {
			case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => if (matchLoop(t1, t2)) true else hasSubsequence(t1, sub) // start matching, if not matched, next
			case (Cons(h1, t1), Cons(h2, t2)) if (h1 != h2) => hasSubsequence(t1, sub) // next
			case _ => false
		}
	}

	// 3.25


	// 3.2

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

}

object main extends App {

	val lst = List(1,2,3,4,5)
	val doubleList = List(1.0, 2.0, 3.0, 0.0, 4.0, 5.0)

	// 3.1
	val x = lst match {
		case Cons(x, Cons(2, Cons(4, _))) => x
		case Nil => 42
		case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
		case Cons(h, t) => h + List.sum(t)
		case _ => 101
	}
	println(x)

	// 3.2
	println(List.tail(lst))

	// 3.3
	println(List.setHead(lst, 6))

	// 3.4
	println(List.drop(lst, 3))

	// 3.5
	println(List.dropWhile(lst, (x: Int) => x < 3))

	// 3.6
	println(List.init(lst))

	// 3.7
	println(List.product2(doubleList))	// 0.0을 만나도 그냥 계속 돈다??

	// 3.8 Ans1 :같은 리스트를 참조하는 객체가 생성
	//foldRight, List 자료 생성자 사이의 관계?? => 같은 부분은 복제하지 않고 참조하는건가..?
	val list1 = List(1,2,3)
	val list2 = List.foldRight(list1, Nil:List[Int])(Cons(_,_))
	println(list2)


	// 3.9
	println(List.length(lst))

	// 3.10
	println(List.foldLeft(lst, 0)(_ + _))
	println(List.foldLeft(lst, 1.0)(_ * _))

	// 3.11
	println(List.sum3(lst))
	println(List.product3(doubleList))

	// 3.12
	println(List.reverse(lst))

	// 3.13 ****

	// 3.14
	println(List.append(lst, 8))

	// 3.15
	println(List.append(List(1,2,3,4), List(5,6,7,8)))

	// 3.16
	val list5 = List.addOne(lst)

	println(list5)

	// 3.17
	println(List.doubleToString(doubleList))

	// 3.18
	println(List.map(list5)(_ * 3))

	// 3.19
	println(List.filter(list5)(_ % 2 == 0))

	// 3.20


	// 3.24
	println(List.hasSubsequence(List(1,2,3,4), List(1,2)))
	println(List.hasSubsequence(List(1,2,3,4), List(1,2,3,4)))
	println(List.hasSubsequence(List(1,2,3,4), List(3,4)))
	println(List.hasSubsequence(List(1,2,3,4), List(1,2,4)))
	println(List.hasSubsequence(List(1,2,3,4), List(3,4,5)))
}