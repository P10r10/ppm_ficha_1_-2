object Main {
  def fun2a(a: (Int, Int), b: (Int, Int)) = (a._1 + b._2, a._1 * b._2)

  def maior(a: Int, b: Int) = if (a > b) a else b

  def menor(a: Int, b: Int) = if (a < b) a else b

  def middle3(x: Int, y: Int, z: Int) = menor(maior(x, y), maior(menor(x, y), z))

  def fun2b(x: Int, y: Int, z: Int) = (maior(maior(x, y), z), middle3(x, y, z))

  def fun2c(x: Int, y: Int, z: Int) = (maior(maior(x, y), z), middle3(x, y, z), menor(menor(x, y), z))

  def ordering1(x: Int, y: Int, z: Int) = {
    val res = fun2b(x, y, z)
    (res._1, res._2, menor(menor(x, y), z))
  }

  def maiorde3(x: Int, y: Int, z: Int) = maior(maior(x, y), z)

  def triangle(x: Int, y: Int, z: Int): Boolean = x + y > z && z + x > y && y + z > x

  def abrev(name: String): String = {
    val split = name.split(" ")
    split.head + " " + split.last
  }

  def exp(x: Int, y: Int): Int = if (y == 0) 1 else x * exp(x, y - 1)

  def func3b(x: List[Int]): (Int, Int) = (x.head, x.last)

  def func3c(x: List[Int]) = (x, x.length)

  def sum(lst: List[Double]): Double = {
    if (lst.isEmpty) 0 else lst.head + sum(lst.tail)
  }

  def func3d(x: List[Double]) = sum(x) / x.length

  def average(lst: List[Double]) = lst.sum / lst.length

  def is_pal[E](lst: List[E]): Boolean = lst == lst.reverse

  def is_pal1[E](lst: List[E]): Boolean = lst match {
    case Nil => true
    case y :: Nil => true
    case y :: ys => if (y == ys.last) is_pal(ys.init) else false
  }

  def rev[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case y :: ys => rev(ys) :+ y
  }

  def transform(lst: List[Int]): List[Int] = lst match {
    case Nil => Nil
    case List(y) => List(y)
    case x :: y :: tail => y :: x :: transform(tail)
  }

  def product(lst: List[Int]): Int = lst match {
    case Nil => 0
    case x :: Nil => x
    case x :: tail => x * product(tail)
  }

  def place_end[A](lst: List[A], x: A): List[A] = lst match {
    case Nil => List(x)
    case y :: tail => y :: place_end(tail, x)
  }

  def concat_lists[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
    case Nil => lst2
    case head :: tail => head :: concat_lists(tail, lst2)
  }

  def sumEl(lst: List[(Int, Int)]) = {
    def aux(lstAux: List[(Int, Int)], index: Int, sum: Int): Int = lstAux match {
      case Nil => sum
      case head :: tail => if (index == 2 || index == 4) aux(tail, index + 1, sum + head._1 + head._2) else
        aux(tail, index + 1, sum)
    }

    aux(lst, 0, 0)
  }

  def average1(lst: List[Double]) = {
    def aux(lst1: List[Double], sum: Double, len: Int): (Int, Double) = lst1 match {
      case Nil => (len, sum)
      case head :: tail => aux(tail, sum + head, 1 + len)
    }

    aux(lst, 0, 0)
  }

  def average3(lst: List[Double]) = average1(lst)._2 / average1(lst)._1

  def metH(lst: List[Double], x: Double): (List[Double], List[Double]) = lst match {
    case Nil => (Nil, Nil)
    case head :: tail => if (head < x) (head :: metH(tail, x)._1, metH(tail, x)._2) else
      (metH(tail, x)._1, head :: metH(tail, x)._2)
  }

  def func_h(lst: List[Double]) = {
    val avg = average3(lst)

    def aux(lst2: List[Double]): List[Double] = lst2 match {
      case Nil => Nil
      case head :: tail => if (head > avg) head :: aux(tail) else
        aux(tail)
    }
    aux(lst)
  }

  def myMeth(lst: List[Double], x: Double): (List[Double], List[Double]) = lst match {
    case Nil => (Nil, Nil)
    case head :: tail => if (head < x) (head :: myMeth(tail, x)._1, myMeth(tail, x)._2) else
      (myMeth(tail, x)._1, head :: myMeth(tail, x)._2)
  }

  type entry = (String, String, String)
  type lTelef = List[entry]

  def emails(lst: lTelef): List[String] = lst match {
      case Nil => Nil
      case (_, _, email) :: tail => email :: (emails(tail))
  }

  def numbersWith2(lst: lTelef): List[String] = lst match {
      case Nil => Nil
      case (_, x, email) :: tail => if (x.head == '2') email :: (numbersWith2(tail)) else
        (numbersWith2(tail))
    }

  def findName(lst: lTelef, name: String): (String, String) = lst match {
    case Nil => ("", "")
    case (nm, ph, em) :: tail => if (name == nm) (nm + findName(tail, name)._1, ph + findName(tail, name)._2)
    else (findName(tail, name)._1, findName(tail, name)._2)
  }

  def divide[A](lst: List[A]) = {
    val half = lst.size / 2.0
    def aux(lst1: List[A], hf: Double, idx: Int): (List[A], List[A]) = lst1 match {
      case Nil => (Nil, Nil)
      case head :: tail => if (idx < hf) (head :: aux(tail, hf, idx + 1)._1, aux(tail, hf, idx + 1)._2) else
        (aux(tail, hf, idx + 1)._1, head :: aux(tail, hf, idx + 1)._2)
    }
    aux(lst, half, 0)
  }

  def main(args: Array[String]): Unit = {

    val myLst = List(("Hugh", "21999", "hugh@iscte"), ("Anna", "22333", "john@iscte"),
                ("Mary", "22666", "mary@iscte"), ("John", "21888", "john@iscte"))
    val lst = List(1, 2, 3, 4, 5, 6)
    println(divide(lst))

  }
}
