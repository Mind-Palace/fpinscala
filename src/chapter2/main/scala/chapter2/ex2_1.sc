def fib(n: Int): Int = {
	@annotation.tailrec
	def loop(n: Int, prev: Int, cur: Int): Int = {
		if (n == 0) prev
		else loop(n - 1, cur, prev + cur)
	}

	loop(n, 0, 1)
}

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
