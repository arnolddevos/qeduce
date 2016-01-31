package qeduce

import java.sql.{Connection, DriverManager, SQLException, ResultSet, PreparedStatement}
import transducers._

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

    def apply(items: SQL*) =
      this ~ sql"(" ~ items.reduceOption( _ ~ sql"," ~ _).getOrElse(sql"") ~ sql")"

    def queryString = parts.mkString("?")
    
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

  trait Query[A] extends (Connection => A) {

    def sql: SQL
    val reducer: Reducer[ResultSet, A]

    def apply(c: Connection): A = withStatement(c) { 
      st =>
        val rs = st.executeQuery
        educe[ResultSet, ResultSet, A](rs, reducer)
    }

    def withStatement(c: Connection)(f: PreparedStatement => A): A = {

      val st = c.prepareStatement(sql.queryString)
      try {
        for((p, i) <- sql.params.zipWithIndex)
          p.sqlType.inject(st, i+1, p.value)
        f(st)
      }
      finally {
        st.close
      }
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

  def withConnection[A](uri: String)( action: Connection => A): A = {
    val c = DriverManager.getConnection(uri)
    c setAutoCommit false
    try {
      val a = action(c)
      c.commit
      a  
    }
    finally {
      c.rollback
      c.close
    }
  } 

}
