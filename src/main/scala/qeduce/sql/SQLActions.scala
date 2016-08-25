package qeduce.sql

import java.sql.{Connection, DriverManager, SQLException, ResultSet, PreparedStatement}
import java.util.Properties
import javax.sql.DataSource
import transducers.{Transducer, Reducer, Educible, Context}
import anodyne.HMaps
import qeduce.generic.Qeduce

trait SQLActions { this: Qeduce with SQLTypes =>

  implicit class SQLContext( sc: StringContext) {
    def sql( ps: QueryValue* ): Query = new Query {
      val parts = sc.parts
      val params = ps
    }
  }

  implicit class SQLOps( val sql: Query ) {
    private def withStatement[A](f: PreparedStatement => A): Action[A] = action {
      c =>
        val st = c.prepareStatement(sql.parts.mkString("?"))
        try {
          for((p, i) <- sql.params.zipWithIndex)
            p.sqlType.inject(st, i+1, p.value)
          f(st)
        }
        finally {
          st.close
        }
    }

    def execute(): Action[Int] = withStatement(_.executeUpdate)

    def map[A]( f: ResultSet => A): Action[Vector[A]] = {
      transduce(transducers.map(f))(transducers.toVector)
    }

    def transduce[A, S](t: Transducer[A, ResultSet])( f: Reducer[A, S]): Action[S] = withStatement {
      st => transducers.transduce(st.executeQuery, t, f): Context[S]
    }

    def reduce[S](f: Reducer[ResultSet, S]): Action[S] = withStatement {
      st => transducers.reduce(st.executeQuery, f): Context[S]
    }
  }

  implicit val resultSetIsEducible = new Educible[ResultSet, ResultSet] {
    def educe[S](rs: ResultSet, f: Reducer[ResultSet, S]): S = {
      var s = f.init
      while(rs.next && ! f.isReduced(s))
        s = f(s, rs)
      f.complete(s)
    }
  }

  trait Action[A] {
    def run(implicit c: Connection): A

    def runWithUrl(url: String, props: Properties = new Properties ) =
      consumeConnection(DriverManager.getConnection(url, props))
    def runwithSource(ds: DataSource) =
      consumeConnection(ds.getConnection)

    def consumeConnection(c: Connection): A = {
      try {
        c setAutoCommit false
        val a = run(c)
        c.commit
        a
      }
      finally {
        c.rollback
        c.close
      }
    }

    def flatMap[B](f: A => Action[B]): Action[B] = action { implicit c => f(run).run }
    def map[B](f: A => B): Action[B] = action { implicit c => f(run) }
    def zip[B]( other: Action[B]):Action[(A, B)] = action { implicit c => (run, other.run) }
    def >>=[B](f: A => Action[B]): Action[B] = flatMap(f)
  }

  def action[A](f: Connection => A): Action[A] = new Action[A] {
    def run(implicit c: Connection): A = f(c)
  }
}
