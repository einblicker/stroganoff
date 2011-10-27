package stroganoff {
  import Stroganoff._
  
  trait Selection { self: Stroganoff =>
    def selection(pool: Seq[Expr]): Seq[Expr]
  }

  trait Roulett extends Selection { self: Stroganoff =>
    def selection(pool: Seq[Expr]): Seq[Expr]
  }
}