package chapter4

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case Some(v) => Some(f(v))
		case None => None
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case Some(v) => v
		case None => default
	}

	def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

	def orElse[B >: A](ob: => Option[B]): Option[B] = if (this != None) this else ob

	def filter(f: A => Boolean): Option[A] = if (this.map(f).getOrElse(false)) this else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

	def mean(xs: Seq[Double]): Option[Double] = if (xs.length == 0) None else Some(xs.sum / xs.length)

	def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
		case (Some(a), Some(b)) => Some(f(a, b))
		case _ => None
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		def loop() = {

		}
	}

}


object main extends App {
	val opt = Some("12")
	val opt2 = None


	println(opt.map(_.toInt))
	println(opt.map(Some(_)))
	println(opt.getOrElse("hh"))
	println(opt2.getOrElse("aa"))

	println(opt.flatMap(Some(_)))
	println(opt.orElse(opt2))
	println(opt2.orElse(opt))

	println(opt.filter(_ == "12"))
	println(opt.filter(_ != "12"))
}

