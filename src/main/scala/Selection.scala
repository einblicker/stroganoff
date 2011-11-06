package stroganoff

import Stroganoff._

trait Selection { self: Stroganoff =>
  def selection(pool: Seq[Expr]): (Double, Seq[Expr])
}

trait Roulette extends Selection { self: Stroganoff =>
  def selection(pool: Seq[self.Expr]): (Double, Seq[self.Expr]) = {
    val poolWithFitness = pool.par.map(expr => (1.0/fitness(expr), expr)).toList.sortBy(- _._1)
    val totalFitness = poolWithFitness.map(_._1).sum
    val accumFitness = poolWithFitness.scanLeft(0.0, Var(names.head):Expr){
      case ((acc, _), (fitness, expr)) =>
	(acc + fitness, expr)
    }.tail
	
    def randomPick(): self.Expr = {
      val pivVal = Random.nextDouble * totalFitness
      val Some((_, expr)) = accumFitness.find(_._1 >= pivVal)
      expr
    }

    import collection.mutable.MutableList
    val nextGeneration = new MutableList[Expr]()
    nextGeneration ++= poolWithFitness.take((popSize * 5.0 / 100.0).toInt).map(_._2)
    while (nextGeneration.length < popSize) {
      def probApp[T](prob: Double, arg: T)(fn: T => T) =
	if (Random.nextDouble() < prob) fn(arg) else arg
      val expr1 = probApp(mutProb, randomPick())(mutation)
      val expr2 = probApp(mutProb, randomPick())(mutation)
      val (newExpr1, newExpr2) =
	probApp(crossProb, (expr1, expr2)){case (a, b) => crossover(a, b)}
      nextGeneration += newExpr1
      nextGeneration += newExpr2
    }
    (totalFitness, nextGeneration)
  }
}
