package qeduce

import java.sql.{DriverManager, PreparedStatement}
import java.util.Properties
import transducers.Reducer

trait SQLActions { this: Qeduce with SQLTypes =>

  implicit class SQLContext( sc: StringContext) {
    def sql( ps: QueryValue* ): Query = new Query {
      val parts = sc.parts
      val params = ps
    }
  }

  private def withStatement[A](q: Query)(f: PreparedStatement => A): Action[A] = action {
    c =>
      val st = c.prepareStatement(q.parts.mkString("?"))
      try {
        for((p, i) <- q.params.zipWithIndex)
          p.sqlType.inject(st, i+1, p.value)
        f(st)
      }
      finally {
        st.close
      }
  }

  def action(q: Query): Action[Int] = withStatement(q)(_.executeUpdate)

  def action[S](q: Query, f: Reducer[Row, S]): Action[S] = withStatement(q) {
    st =>
      val rs = st.executeQuery
      var s = f.init
      while(! f.isReduced(s) && rs.next)
        s = f(s, rs)
      f.complete(s)
  }

  def consumeConnection[A](aa: Action[A]): Action[A] = action {
    c =>
      try {
        c setAutoCommit false
        val a = aa.run(c)
        c.commit
        a
      }
      finally {
        c.rollback
        c.close
      }
  }

  implicit class ActionOps[A](aa: Action[A]) {
    def runWithUrl(url: String, props: Properties = new Properties ): A =
      consumeConnection(aa).run(DriverManager.getConnection(url, props))
  }
}
