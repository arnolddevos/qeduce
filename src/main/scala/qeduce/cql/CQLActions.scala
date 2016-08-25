package qeduce.cql

import qeduce.generic.Qeduce
import transducers.{Transducer, Reducer, count}

trait CQLActions { this: Qeduce with CQLTypes =>

  implicit class CQLContext( sc: StringContext) {
    def cql( ps: QueryValue* ): Query = new Query {
      val parts = sc.parts
      val params = ps
    }
  }

  implicit class CQLOps( val q: Query ) {

    def execute(): Action[Int] = reduce(count)

    def map[A]( f: Row => A): Action[Vector[A]] =
      transduce(transducers.map(f))(transducers.toVector)

    def transduce[A, S](t: Transducer[A, Row])( f: Reducer[A, S]): Action[S] =
      reduce(t(f))

    def reduce[S](f: Reducer[Row, S]): Action[S] = action {
      session =>
        val statement = session.prepare(q.parts.mkString("?")).bind()

        for((p, i) <- q.params.zipWithIndex)
          p.sqlType.inject(statement, i, p.value)

        val result = session.execute(statement)

        var s = f.init
        while(! f.isReduced(s) && ! result.isExhausted)
          s = f(s, result.one())
        f.complete(s)
    }
  }

  trait Action[A] {
    def run(implicit c: Session): A

    def consumeConnection(c: Session): A = {
      try {
        run(c)
      }
      finally {
        c.close
      }
    }

    def flatMap[B](f: A => Action[B]): Action[B] = action { implicit c => f(run).run }
    def map[B](f: A => B): Action[B] = action { implicit c => f(run) }
    def zip[B]( other: Action[B]):Action[(A, B)] = action { implicit c => (run, other.run) }
    def >>=[B](f: A => Action[B]): Action[B] = flatMap(f)
  }

  def action[A](f: Session => A): Action[A] = new Action[A] {
    def run(implicit c: Session): A = f(c)
  }
}
