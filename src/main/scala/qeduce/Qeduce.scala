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

  implicit class SQLofTerm(t: SQLTerm) extends SQL {
    val parts = Seq("\""+t.name+"\"")
    val params = Seq()
  }

  trait SQLType[A] {
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

  trait SQLTerm {
    type Value
    def name: String
    def sqlType: SQLType[Value]
    def apply()(implicit r: Row): Value = r(this)
    def unapply( r: Row ): Option[Value] = r.get(this)
    override def toString = "'" + name
  }

  def term[A](n: String)(implicit t: SQLType[A]) = new SQLTerm {
    type Value = A
    val name = n
    val sqlType = t
  }

  def term[A](s: Symbol)(implicit t: SQLType[A]): SQLTerm { type Value = A } = term(s.name)

  trait Row {
    def get(t: SQLTerm): Option[t.Value]
    def apply(t: SQLTerm): t.Value
    def project(ts: SQLTerm*): Row
  }
}
