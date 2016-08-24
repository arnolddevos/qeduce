package qeduce.generic

import anodyne.HMaps

trait Qeduce { this: HMaps =>

  type Statement
  type Row

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

  implicit class SQLofTerm(t: Term) extends SQL {
    val parts = Seq("\""+t.name+"\"")
    val params = Seq()
  }

  trait SQLType[A] {
    def extract: (Row, String) => A
    def tryExtract: (Row, String) => Option[A]
    def inject: (Statement, Int, A) => Unit
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

  implicit class RowOps(rs: Row) {
    def get(t: Term)(implicit e: SQLType[t.Value]): Option[t.Value] = e.tryExtract(rs, t.name)
    def apply(t: Term)(implicit e: SQLType[t.Value]): t.Value = e.extract(rs, t.name)
    def get[A](c: Symbol)( implicit e: SQLType[A]): Option[A] = e.tryExtract(rs, c.name)
    def apply[A](c: Symbol)( implicit e: SQLType[A]): A = e.extract(rs, c.name)
  }

  abstract class Term extends TermSpec {
    def name: String
    def apply()(implicit rs: Row, e: SQLType[Value]): Value = e.extract(rs, name)
    def unapply(rs: Row)(implicit e: SQLType[Value]): Option[Value] = e.tryExtract(rs, name)
    // def apply()(implicit h: HMap): Value = h(this)
    def unapply(h: HMap): Option[Value] = h.get(this)
    override def toString = "'" + name
  }

  def term[A](n: String) = new Term {
    type Value = A
    val name = n
  }

  def term[A](s: Symbol): Term { type Value = A } = term(s.name)

}
