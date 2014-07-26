package org.jrosalia

abstract class SemiRing[A] {
  def add(x: A, y: A): A
  def multiply(x: A, y: A): A
  def zero: A
  def identity: A
}

import Matrix._
class Matrix(val n: Int, val m: Int, val data: Seq[Value], val args: Map[String,Double] = Map.empty[String,Double])(implicit op: SemiRing[Value]) {
  require(data.length == n * m)

  def apply(r:Int, c:Int) = data(r + c * n)

  def apply(args: Map[String, Double]) =
    new Matrix(n, m, resolve(args).map(liftToFunction(_)))

  def columnOrder[A](rows:Int, cols:Int)(f: (Int,Int) => A) =
    for (j <- 0 until cols; i <- 0 until rows) yield f(i,j)
  
  def +(that: Value) =
    new Matrix(n, m, columnOrder(n,m) {(i,j) => op.add(this(i, j), that)}, args)

  def *(that: Value) =
    new Matrix(n, m, columnOrder(n,m) {(i,j) => op.multiply(this(i, j), that)}, args)

  def +(that: Matrix) = {
    require(this.n == that.n && this.m == that.m)
    new Matrix(n, m, columnOrder(n,m) {(i,j) => op.add(this(i, j), that(i,j))}, args)
  }

  def *(that: Matrix) = {
    require(this.m == that.n)
    new Matrix(n, that.m,
      columnOrder(n, that.m) {
        (i,j) =>
          (for (k <- 0 until m)
            yield op.multiply(this(i, k), that(k, j))
          ).reduce(op.add(_, _))
      }, args ++ that.args)
  }

  def t = new Matrix(m, n, columnOrder(m, n) {(i, j) => this(j, i)}, args)

  //TODO: support partial resolution
  def resolve(args: Map[String, Double]): Seq[Double] =
     columnOrder(n, m) {(i,j) => this(i, j)(args)}
    
  override def toString =
    resolve(args).grouped(n).toList.transpose.map(_.mkString(" ")).mkString("\n")
}

object Matrix {

  type Value = Map[String,Double] => Double

  implicit class FuncWithArgs(f: Double => Double) {
    
    def apply(arg: String) =
      (args:Map[String,Double]) => f(args(arg))
  }
  
  implicit class FuncWithNeg(g: Map[String,Double] => Double) {
    def unary_- : Map[String,Double] => Double = v => -g(v)
  }

  import scala.language.implicitConversions
  implicit def liftToFunction(v: Double): Value = _ => v

  implicit object NamedArgFunctionSemiRing extends SemiRing[Value] {

    override def zero = {
      _ => 0.0
    }

    override def identity = {
      _ => 1.0
    }

    override def add(f1: Value, f2: Value) = {
      args => f1(args) + f2(args)
    }

    override def multiply(f1: Value, f2: Value) = {
      args => f1(args) * f2(args)
    }
  }
}

object MatrixTest extends App {
  
    //  implicit def wrapArg1(v: Double => Double): (Map[String,Double] => Double) = args => v(0)
  import Math._
//  val asdf = FuncWithArgs(cos)("theta")
//  val bsdf = - (Math.cos _ & "theta")
  val I = new Matrix(2, 2, List(1.0, 0.0, 0.0, 1.0))
  val A = new Matrix(2, 1, List(1.0, 1.0))
  //2D rotation matrix
  val B = new Matrix(2, 2, List((Math.cos _)("theta"), (Math.sin _)("theta"), -(Math.sin _)("theta"), (Math.cos _)("theta")))
//  val C = new Matrix(2, 2, List((Math.cos _)("theta2"), (Math.sin _)("theta2"), -(Math.sin _)("theta2"), (Math.cos _)("theta2")))
  //TODO: remap B's arguments, to create new matrix from B with same structure but different arguments
//  B.remap("theta" -> "theta2")
//  println(A + 5.0)
//  println
//  println(B(Math.PI/2) * I)
//  println
//  println((B * I)(Math.PI/2))
//  println
  val a1 = Map("theta" -> Math.PI/2)
  println(B(a1) * A)
//  println
//  List.fill(4)(Math.PI/2)
  println((B * A)(a1))
  println
  //full rotation
  println(B(a1) * B(a1) * B(a1) * B(a1) * A)
  println((B * B * B * B)(a1) * A)
  
  val a2 = Map("theta" -> 2*Math.PI)
  println(B(a2) * A)

  println(B(a1))
  println(B(a1) t)
  
  println(A t)
  println(A * 0)
  println(A * 0)
}