package qeduce

import java.sql.{ResultSet, PreparedStatement}

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
    val params = Seq(new SQLCapture(a))
  }

  implicit class SQLofSymbol(s: Symbol) extends SQL {
    val parts = Seq("\""+s.name+"\"")
    val params = Seq()
  }

  implicit class SQLofTerm(t: SQLTerm[_]) extends SQL {
    val parts = Seq("\""+t.name+"\"")
    val params = Seq()
  }

  trait SQLType[A] extends (((ResultSet, Symbol)) => A) {
    final def apply(h: (ResultSet, Symbol)): A = extract(h._1, h._2.name)
    def extract: (ResultSet, String) => A
    def inject: (PreparedStatement, Int, A) => Unit
    def display: A => String
  }

  trait SQLValue {
    type Value
    def value: Value
    def sqlType: SQLType[Value]
    override def toString = "${"+sqlType.display(value)+"}"
  }

  implicit class SQLCapture[A](a: A)(implicit t: SQLType[A]) extends SQLValue {
    type Value = A
    val value = a
    val sqlType = t
  }

  trait SQLTerm[A] {
    def name: String
    def sqlType: SQLType[A]
    def apply()(implicit rs: ResultSet): A = sqlType.extract(rs, name)
    override def toString = "'" + name
  }

  def term[A](n: String)(implicit t: SQLType[A]): SQLTerm[A] = new SQLTerm[A] {
    val name = n
    val sqlType = t
  }

  def term[A](s: Symbol)(implicit t: SQLType[A]): SQLTerm[A] = term(s.name)

  implicit class SQLRowView(rs: ResultSet) {
    def apply[A](c: Symbol)( implicit t: SQLType[A]): A = t.extract(rs, c.name)
    def apply[A](t: SQLTerm[A]): A = t.sqlType.extract(rs, t.name)
  }
}
