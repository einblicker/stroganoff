package stroganoff

import scalax.file.Path
import org.apache.commons.math.linear.Array2DRowRealMatrix

object Random extends java.util.Random

abstract class Stroganoff(
  val popSize: Int,
  val crossProb: Double,
  val mutProb: Double,
  val weight: Double = 1.0
) {

  val path = """sample.csv"""

  type Factors = (Double, Double, Double, Double, Double, Double)

  sealed trait Expr
  case class Var(name: String) extends Expr
  case class Node(
    var factors: Factors,
    var fn: Option[Seq[Double] => Double],
    lhs: Expr,
    rhs: Expr
  ) extends Expr
  
  val names: Seq[String] = (0 to 9).map("x" + _.toString)
  val data: Seq[Double] =
    Path(path).lines().toSeq.map(_.split(",")(4).toDouble).take(500)
  val validationData: Seq[Double] =
    Path(path).lines().toSeq.map(_.split(",")(4).toDouble).drop(500).take(500)

  val period = 30
  
  def genExpr(depth: Int = 3): Expr = {
    def genVar() = 
      Var(names(Random.nextInt(names.length)))
    def genNode() = {
      val expr =
	Node(
          (0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
          None,
          genExpr(depth - 1),
          genExpr(depth - 1)
        )
      setFactorAndFn(expr)
      expr
    }
    Random.nextDouble() match {
      case x if x < 0.5 || depth <= 0 =>
	genVar()
      case _ =>
	genNode()
    }
  }
  
  def mutation(expr: Expr): Expr = 
    if (Random.nextDouble() < 0.5)
      genExpr()
    else
      expr match {
	case expr@Var(_) =>
          expr
	case Node(factors, fn, lhs, rhs) =>
	  Node(
            factors,
	    fn,
            mutation(lhs),
            mutation(rhs)
          )
      }
  
  def crossover(
    expr1: Expr,
    expr2: Expr
  ): (Expr, Expr) = {
    import Susp._
    
    var haltFn = () => Random.nextDouble() < 0.5
    def suspendableIter(expr: Expr): Susp[Expr] =
      expr match {
	case _ if haltFn() => for {
	  newExpr <- suspend(expr)
	 } yield newExpr
	case Var(_) =>
	  Result(expr)
	case Node(factors, fn, lhs, rhs) => for {
	  lhs <- suspendableIter(lhs)
	  rhs <- suspendableIter(rhs)
	 } yield Node(factors, fn, lhs, rhs)
      }
  
    def recur(susp1: Susp[Expr], susp2: Susp[Expr]): (Expr, Expr) =
      (susp1, susp2) match {
	case (Cont(sub1, cont1), Cont(sub2, cont2)) =>
          recur(cont1(sub2), cont2(sub1))
        case (Cont(sub1, cont1), expr@Result(_)) =>
	  recur(cont1(sub1), expr)
        case (expr@Result(_), Cont(sub2, cont2)) =>
	  recur(expr, cont2(sub2))
        case (Result(expr1), Result(expr2)) =>
	  (expr1, expr2)
      }
    
    val susp1 = suspendableIter(expr1)
    val susp2 = suspendableIter(expr2)
    haltFn = () => false
    val (newExpr1, newExpr2) = recur(susp1, susp2)
    setFactorAndFn(newExpr1)
    setFactorAndFn(newExpr2)
    (newExpr1, newExpr2)
  }

  def setFactorAndFn(expr: Expr): Unit = {
    def calc(z1: Seq[Double], z2: Seq[Double], y: Seq[Double]) = {
      val X = new Array2DRowRealMatrix(
	(z1 zip z2).map{
          case (a, b) =>
	    Array(1.0, a, b, a*b, a*a, b*b)
	}.toArray)
      val Xt = X.transpose
      val XtX = Xt.multiply(X)
      if (XtX.isSingular) Array(Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
      else XtX.inverse.multiply(Xt).
      multiply(new Array2DRowRealMatrix(y.toArray.drop(names.length - 1))).
      transpose.getData()
    }

    def recur(expr: Expr): Seq[Double] => Double =
      expr match {
	case Var(name) =>
          val index = name.last.toInt - '0'.toInt
	  vars => vars(index)
	case expr@Node(_, _, lhs, rhs) =>
	  val lhsFn = recur(lhs)
	  val rhsFn = recur(rhs)
	  val z1 = data.sliding(names.length).map(lhsFn).toSeq
          val z2 = data.sliding(names.length).map(rhsFn).toSeq
          val Array(Array(a0, a1, a2, a3, a4, a5)) = calc(z1, z2, data)
          expr.factors = (a0, a1, a2, a3, a4, a5)
	  val fn: Seq[Double] => Double = vars => {
	    val x1 = lhsFn(vars)
	    val x2 = rhsFn(vars)
	    a0 + a1*x1 + a2*x2 + a3*x1*x2 + a4*x1*x1 + a5*x2*x2
          }
          expr.fn = Some(fn)
          fn
      }
    
    recur(expr)
    ()
  }
  
  def fitness(expr: Expr): Double = {
    import math._
    
    def compile(expr: Expr): Seq[Double] => Double =
      expr match {
	case Var(name) =>
          val index = name.last.toInt - '0'.toInt
	  (vars => vars(index))
        case Node((a0, a1, a2, a3, a4, a5), Some(fn), lhs, rhs) =>
	  fn
        case Node((a0, a1, a2, a3, a4, a5), None, lhs, rhs) =>
	  val lhsFn = compile(lhs)
          val rhsFn = compile(rhs)
          vars => {
	    val x1 = lhsFn(vars)
	    val x2 = rhsFn(vars)
	    a0 + a1*x1 + a2*x2 + a3*x1*x2 + a4*x1*x1 + a5*x2*x2
	  }
     }

    def countParam(expr: Expr): Int =
      expr match {
	case Var(_) => 0
	case Node(_, _, lhs, rhs) =>
	  6 + countParam(lhs) + countParam(rhs)
      }
    
    val x1 = data.sliding(names.length).map(compile(expr)).toSeq
    val x2 = data.dropRight(names.length - 1).drop(period)
    val N = x2.length.toDouble
    val S = ((x1 zip x2).map{case (x, y) => pow(x - y, 2.0)}.sum + 1) / N
    val R = countParam(expr).toDouble
    0.5*N*log(S + 1) + weight*0.5*R*log(N + 1)
  }

  def selection(pool: Seq[Expr]): (Double, Seq[Expr])

  def evolve(): Expr = {
    var pool: Seq[Expr] = (1 to popSize).map(_ => genExpr())
    for(_ <- 1 to 20) {
      val (totalFitness, nextGeneration) = selection(pool)
      pool = nextGeneration
      println("average of reciprocal of fitness:" + totalFitness/popSize)
    }
    pool.sortBy(fitness).head
  }
}

object Stroganoff {
  def main(args: Array[String]): Unit = {
    val sg = new Stroganoff(1000, 0.6, 0.6, 0.001) with Roulette
    import sg._
    val expr = sg.evolve()
    println(expr)
    val Node(_, Some(fn), _, _) = expr
    val z1 = validationData.dropRight(names.length - 1).drop(period).toSeq
    val z2 = validationData.sliding(names.length).map(fn).toSeq
    (z1 zip z2).foreach{case (a, b) => println(""+a+","+b)}
  }
}
