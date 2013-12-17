package org.jrosalia

abstract class SemiRing[A] {
  def add(x: A, y: A): A
  def multiply(x: A, y: A): A
  def zero: A
  def identity: A
}

import Matrix._
class Matrix(val n: Int, val m: Int, val data: Seq[Value], val arg: Option[Double] = None)(implicit op: SemiRing[Value]) {
  require(data.length == n * m)

  def apply(v: Double) =
    new Matrix(n, m, resolve(v).flatten.map(liftToFunction(_)))

  def +(that: Value) =
    new Matrix(n, m,
      (for (j <- 0 until m; i <- 0 until n) yield
          op.add(data(i + j * n), that)), arg)

  def *(that: Value) =
    new Matrix(n, m,
      (for (j <- 0 until m; i <- 0 until n) yield
          op.multiply(data(i + j * n), that)), arg)

  def +(that: Matrix) = {
    require(this.n == that.n && this.m == that.m)
    new Matrix(n, that.m, 
      (for (j <- 0 until that.m; i <- 0 until n) yield
          op.add(data(i + j * n), that.data(i + j * n))), arg)
  }

  def *(that: Matrix) = {
    require(this.m == that.n)
    new Matrix(n, that.m,
      (for (j <- 0 until that.m; i <- 0 until n) yield (for (k <- 0 until m) yield {
        val thisInx = i + k * n;
        val thatInx = k + j * that.m;
        op.multiply(data(thisInx), that.data(thatInx))
      }).reduce(op.add(_, _))), arg)
  }

  def t = new Matrix(m, n, (for (i <- 0 until n; j <- 0 until m) yield data(i + j * n)), arg)

  def resolve(v: Double) =
    for (i <- 0 until n) yield for (j <- 0 until m) yield data(i + j * n)(v)
    
  override def toString = {
    resolve(arg.getOrElse(0)).map(_.mkString(" ")).mkString("\n")
  }
}

object Matrix {

  type Value = Double => Double

  import scala.language.implicitConversions
  implicit def liftToFunction(v: Double): Value = _ => v

//  implicit class ArithmeticFun(f: Unit => Double) {
//    def + (that: Unit => Double) = {
//      _ => f() + that()
//    }
//  }
  implicit object FunctionSemiRing extends SemiRing[Value] {

    override def zero = {
      _ => 0.0
    }

    override def identity = {
      _ => 1.0
    }

    override def add(f1: Value, f2: Value) = {
      v => f1(v) + f2(v)
    }

    override def multiply(f1: Value, f2: Value) = {
      v => f1(v) * f2(v)
    }
  }
}

object MatrixTest extends App {
  
  val A = new Matrix(2, 1, List(1.0, 1.0))
  //2D rotation matrix
  val B = new Matrix(2, 2, List(Math.cos _, Math.sin _, v => -Math.sin(v), Math.cos _))
  
  println(A + 5.0)
  println

  println(B(Math.PI/2) * A)
  //full rotation
  println(B(Math.PI/2) * B(Math.PI/2) * B(Math.PI/2) * B(Math.PI/2) * A)
  
  println(B(2 * Math.PI) * A)

  println (B(Math.PI/2) t)
  
  println(A * 0)
  println(A * 0)
}