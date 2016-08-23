package qeduce

import java.sql.{Connection, DriverManager, SQLException, ResultSet, PreparedStatement}
import java.util.Properties
import javax.sql.DataSource
import transducers.{Transducer, Reducer, Educible, Context}
import anodyne.HMaps

trait Effects { this: Qeduce with HMaps =>

  implicit class SQLOps( val sql: SQL ) {
    private def withStatement[A](f: PreparedStatement => A): Effect[A] = effect {
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

    def update: Effect[Int] = withStatement(_.executeUpdate)

    def map[A]( f: ResultSet => A): Effect[Vector[A]] = {
      transduce(transducers.map(f))(transducers.toVector)
    }

    def transduce[A, S](t: Transducer[A, ResultSet])( f: Reducer[A, S]): Effect[S] = withStatement {
      st => transducers.transduce(st.executeQuery, t, f): Context[S]
    }

    def reduce[S](f: Reducer[ResultSet, S]): Effect[S] = withStatement {
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

  trait Effect[A] {
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

    def flatMap[B](f: A => Effect[B]): Effect[B] = effect { implicit c => f(run).run }
    def map[B](f: A => B): Effect[B] = effect { implicit c => f(run) }
    def zip[B]( other: Effect[B]):Effect[(A, B)] = effect { implicit c => (run, other.run) }
    def >>=[B](f: A => Effect[B]): Effect[B] = flatMap(f)
  }

  def effect[A](f: Connection => A): Effect[A] = new Effect[A] {
    def run(implicit c: Connection): A = f(c)
  }
}
