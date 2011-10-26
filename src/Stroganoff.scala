import scalax.file
import file.Path
import org.apache.commons.math.linear._
package stroganoff {
  
  object Random extends java.util.Random

  object SuspMonad {
    sealed trait Susp[A] {
      def map[B](f: A => B): Susp[B] = flatMap(x => Result(f(x)))
      def flatMap[B](f: A => Susp[B]): Susp[B]
    }
    
    case class Cont[A, E, V](
      value: E,
      cont: V => Susp[A]
    ) extends Susp[A] {
      def flatMap[B](f: A => Susp[B]): Susp[B] =
	Cont(value, (val0: V) => cont(val0).flatMap(f))
    }
    
    case class Result[A](value: A) extends Susp[A] {
      def flatMap[B](f: A => Susp[B]): Susp[B] =
	f(value)
    }
    
    def suspend[A](value: A): Susp[A] = Cont(value, Result.apply)
  }
  
  /*
class Stroganoff(
  popSize: Int,
  crossProb: Double,
  mutProb: Double
  )*/
  object Stroganoff {
    
    type Factor = (Double, Double, Double, Double, Double, Double)

    sealed trait Expr
    case class Var(name: String) extends Expr
    case class Node(var factor: Factor, lhs: Expr, rhs: Expr) extends Expr
    
    val names: Seq[String] = 0 to 9 map {"x"+_.toString}
    val data: Seq[Double] =
      Path("../test/test_data.csv").lines().toList.map(_.split(",")(4).toDouble)
    
    def genExpr(depth: Int = 3): Expr = {
      def genVar() = 
	Var(names(Random.nextInt(names.length)))
      def genNode() = {
	val expr = Node((0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
			genExpr(depth - 1),
			genExpr(depth - 1))
	calcFactor(expr)
	expr
      }
      Random.nextDouble() match {
	case x if x < 0.3 || depth <= 0 =>
          genVar()
	case _ =>
          genNode()
      }
    }
    
    def mutation(expr: Expr): Expr = 
      if (Random.nextDouble() < 0.1) {
	val expr = genExpr()
	calcFactor(expr)
	expr
      } else
	expr match {
          case expr@Var(_) =>
            expr
	  case Node(factor, lhs, rhs) =>
            Node(factor,
	         mutation(lhs),
	         mutation(rhs))
       }

    def crossover(
      expr1: Expr,
      expr2: Expr
    ): (Expr, Expr) = {
      def suspendableIter(expr: Expr): Susp[Expr] =
	expr match {
          case _ if Random.nextDouble() < 0.1 =>
	    for {
	      newExpr <- suspend(expr)
              _ = calcFactor(newExpr)
	    } yield newExpr
          case Var(_) =>
            Result(expr)
	  case Node(factor, lhs, rhs) =>
            for {
	      lhs <- suspendableIter(lhs)
	      rhs <- suspendableIter(rhs)
	    } yield Node(factor, lhs, rhs)
        }
    
      lazy val recur: (Susp[Expr], Susp[Expr]) => (Expr, Expr) = {
        case (Cont(sub1, cont1), Cont(sub2, cont2)) =>
          recur(cont1(sub2), cont2(sub1))
        case (Cont(sub1, cont1), expr@Result(_)) =>
	  recur(cont1(sub1), expr)
        case (expr@Result(_), Cont(sub2, cont2)) =>
	  recur(expr, cont2(sub2))
        case (Result(expr1), Result(expr2)) =>
          (expr1, expr2)
      }

      recur(suspendableIter(expr1),
        suspendableIter(expr2))
    }

    def calcFactor(expr: Expr) = {
      def calc(z1: Seq[Double], z2: Seq[Double], y: Seq[Double]) = {
        val X = new Array2DRowRealMatrix(
          z1 zip z2 map {
            case (a, b) =>
	      Array(1.0, a, b, a*b, a*a, b*b)
          } toArray)
        val Xt = X.transpose
        Xt.multiply(X).
        inverse.multiply(Xt).
        multiply(new Array2DRowRealMatrix(y.toArray.drop(9))).
        transpose
      }

      def recur(expr: Expr): Seq[Double] => Double =
        expr match {
          case Var(name) =>
            val index = name.last.toInt - '0'.toInt
	    ((vars: Seq[Double]) => vars(index))
	  case expr@Node(factor, lhs, rhs) =>
	    val lhsFn = recur(lhs)
	    val rhsFn = recur(rhs)
	    val z1 = data sliding{10} map{lhsFn(_:Seq[Double])}
            val z2 = data sliding{10} map{rhsFn(_:Seq[Double])}
            val Array(Array(a0, a1, a2, a3, a4, a5)) = calc(z1 toSeq, z2 toSeq, data).getData()
            expr.factor = (a0, a1, a2, a3, a4, a5)
            ((vars: Seq[Double]) => {
	      val x1 = lhsFn(vars)
	      val x2 = rhsFn(vars)
	      a0 + a1*x1 + a2*x2 + a3*x1*x2 + a4*x1*x1 + a5*x2*x2
	    })
        }
      
      recur(expr)
    }

    def fitness(expr: Expr): Double = {
      lazy val compile: Expr => (Seq[Double] => Double) = {
        case Var(name) =>
          val index = name.last.toInt - '0'.toInt
	  (vars => vars(index))
        case Node((a0, a1, a2, a3, a4, a5), lhs, rhs) =>
	  val lhsFn = compile(lhs)
          val rhsFn = compile(rhs)
          (vars => {
	    val x1 = lhsFn(vars)
	    val x2 = rhsFn(vars)
	    a0 + a1*x1 + a2*x2 + a3*x1*x2 + a4*x1*x1 + a5*x2*x2
	  })
      }
      val fn = compile(expr)
      data sliding{10} map{fn(_:Seq[Double])}
      throw new Exception()
    }
  
    def crossoverRun() = {
      val expr1 = genExpr()
      val expr2 = genExpr()
      (expr1, expr2)==crossover(expr1, expr2)
    }
  
    def main(args: Array[String]) = {
      val expr1 = genExpr()
      val expr2 = genExpr()
      println((expr1, expr2)==crossover(expr1, expr2))
    }
  }
}
