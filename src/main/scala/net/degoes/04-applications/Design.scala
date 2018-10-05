package net.degoes.applications

object Design {
  import scalaz.zio._

  sealed abstract class DatabaseError extends Exception
  trait Source[E, A] {
    def fold[Z](z: Z)(f: (Z, A) => Z): IO[E, Z]
  }
  type Database[A] = IO[DatabaseError, A]
  type DatabaseSource[A] = Source[DatabaseError, A]
  type DatabaseDerived[A, B] = DatabaseSource[A] => Database[B]

  trait Number[A] {
    def zero: A
    def one: A
    def plus(l: A, r: A): A
    def minus(l: A, r: A): A
    def times(l: A, r: A): A
  }
  object Number {
    def apply[A](implicit N: Number[A]): Number[A] = N
  }
  implicit class NumberSyntax[A](l: A) {
    def + (r: A)(implicit N: Number[A]): A = N.plus(l, r)
    def - (r: A)(implicit N: Number[A]): A = N.minus(l, r)
    def * (r: A)(implicit N: Number[A]): A = N.times(l, r)
  }

  final case class Customer[AccountID, Num](
                                             name: String,
                                             email: String,
                                             account: Account[AccountID, Num]
                                           )
  final case class Account[AccountID, Num](
                                            id  : AccountID,
                                            txns: DatabaseSource[Transaction[AccountID, Num]])
  object Account {
    import Transaction._
    type TxnDerived[A, B] = DatabaseDerived[Transaction[A, B], B]

    def balance[A, B: Number] : TxnDerived[A, B] =
      _.fold[B](Number[B].zero) {
        case (balance, Redeem  (v, _)) => balance - v
        case (balance, Earn    (v, _)) => balance + v
        case (balance, Transfer(v, _)) => balance - v
      }
    def status[A, B] : TxnDerived[A, Status] =
      _.fold[Status](Status.Open) {
        case (status, _) => status
      }

    def tier[A, B: Number: Order](tiers: Map[B, Tier]) : TxnDerived[A, B] =
      ???

    sealed trait Status
    object Status {
      case object Open extends Status
      case object Closed extends Status
    }
    sealed trait Tier
    object Tier {
      case object Silver   extends Tier
      case object Gold     extends Tier
      case object Platinum extends Tier
    }
  }
  final case class Reward()
  final case class Purchase(id: java.util.UUID, description: String, quantity: Int)

  sealed trait Transaction[+AccountID, +Num]
  object Transaction {
    final case class Redeem   [Num](amount: Num, reward: Reward) extends Transaction[Nothing, Num]
    final case class Earn     [Num](amount: Num, purchase: Purchase) extends Transaction[Nothing, Num]
    final case class Transfer [AccountID, Num](amount: Num, recipient: AccountID) extends Transaction[AccountID, Num]
  }

  object hoas {
    /**
      * FOAS:
      * 1. embed DSL into programming language
      * 2. With data structure (initial) or type class (final)
      */

    trait Expr[A] {
      def int(v: Int): A
      def plus(l:A, r:A): A
      def times(l:A, r:A): A
    }
  }
}
