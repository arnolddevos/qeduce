package qeduce

import java.sql.{Connection, DriverManager, SQLException, ResultSet, PreparedStatement}
import javax.sql.DataSource
import transducers.{Transducer, Reducer, Educible, Context}

trait Qeduce {

  trait SQL {
    def parts: Seq[String]
    def params: Seq[SQLValue]

    def ~(other: SQL): SQL = {
      val a = parts dropRight 1
      val b = parts.last + " " + other.parts.head
      val c = other.parts drop 1
      val d = params ++ other.params
      new SQL{
        val parts = (a :+ b) ++ c
        val params = d
      }
    }
    
    override def toString =
      parts.zip(params).map { case (s, p) => s+p }.mkString + parts.last
  }

  implicit class SQLContext( sc: StringContext) {
    def sql( ps: SQLValue* ): SQL = new SQL {
      val parts = sc.parts
      val params = ps
    }
  }

  implicit class SQLofValue[A](a: A)(implicit t: SQLType[A]) extends SQL {
    val parts = Seq("", "")
    val params = Seq(new SQLValueConcrete(a))
  }

  implicit class SQLofSymbol(s: Symbol) extends SQL {
    val parts = Seq("\""+s.name+"\"")
    val params = Seq()
  }

  trait SQLType[A] extends (((ResultSet, Symbol)) => A) {
    final def apply(h: (ResultSet, Symbol)): A = extract(h._1, h._2.name)
    def extract: (ResultSet, String) => A
    def inject: (PreparedStatement, Int, A) => Unit
    def display: A => String
  }

  trait SQLValue {
    type A
    def value: A
    def sqlType: SQLType[A]
    override def toString = "${"+sqlType.display(value)+"}"
  }

  implicit class SQLValueConcrete[A1](a: A1)(implicit t: SQLType[A1]) extends SQLValue {
    type A = A1
    val value = a
    val sqlType = t
  }

  implicit class SQLOps( val sql: SQL ) {
    def withStatement[A](f: PreparedStatement => A): Effect[A] = effect { 
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

    def runWithUrl(uri: String) = consumeConnection(DriverManager.getConnection(uri))
    def runwithSource(ds: DataSource) = consumeConnection(ds.getConnection)
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

    def flatMap[B](f: A => Effect[B]) = effect { implicit c => f(run).run }

    def map[B](f: A => B) = effect { implicit c => f(run) }

    def zip[B]( other: Effect[B]) = effect { implicit c => (run, other.run) }

    def >>=[B](f: A => Effect[B]) = flatMap(f)
  }

  def effect[A](f: Connection => A): Effect[A] = new Effect[A] {
    def run(implicit c: Connection): A = f(c)
  }
}
