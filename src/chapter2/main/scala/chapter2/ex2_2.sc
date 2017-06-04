object ex2_2 {

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

	def loop(arr: Array[A]): Boolean = {
		if (arr.isEmpty || arr.tail.isEmpty) true
		else if (ordered(arr.head, arr.tail.head)) loop(arr.tail)
		else false
	}

	loop(as)
}

	def ascInt(x: Int, y: Int): Boolean = x <= y
	def descInt(x: Int, y: Int): Boolean = x >= y


	isSorted(Array(1,2,3,4,5), ascInt)
	isSorted(Array(1,3,2,4,5), ascInt)
	isSorted(Array(5,4,3,2,1), descInt)
	isSorted(Array(1,3,2,4,5), descInt)
}

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

	def loop(arr: Array[A]): Boolean = {
		if (arr.isEmpty || arr.tail.isEmpty) true
		else if (ordered(arr.head, arr.tail.head)) loop(arr.tail)
		else false
	}

	loop(as)
}