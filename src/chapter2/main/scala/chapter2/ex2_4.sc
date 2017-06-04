object ex2_5 {
def compose[A, B, C](f: B => C, g: A => B): A => C = {
	(a: A) => f(g(a))
}

	def define1(a: Int, b: Int) = a + b
	def define2 = (a: Int, b: Int) => a + b
	def define3(a: Int, b: Int) = (a: Int, b: Int) => a + b
	val values1 = (a: Int, b: Int) => a + b

}