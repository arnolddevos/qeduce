package qeduce

import transducers.api.{Transducer, Reducer, map => mapOp, toVector}
import scala.language.higherKinds

trait Qeduce {

  type Session
  type Statement
  type Row

  trait Query {
    def parts: Seq[String]
    def params: Seq[QueryValue]

    def ~(other: Query): Query = {
      val a = parts dropRight 1
      val b = parts.last + " " + other.parts.head
      val c = other.parts drop 1
      val d = params ++ other.params
      new Query{
        val parts = (a :+ b) ++ c
        val params = d
      }
    }

    def update(): Action[Int] = action(this)
    def reduce[S](f: Reducer[Row, S]): Action[S] = action(this, f)
    def map[A]( f: Row => A): Action[Vector[A]] = transduce(mapOp(f))(toVector)
    def transduce[A, S](t: Transducer[A, Row])( f: Reducer[A, S]): Action[S] = reduce(t(f))

    override def toString = parts.zip(params).map { case (s, p) => s+p }.mkString + parts.last
  }

  trait QueryValue {
    type Value
    def value: Value
    def sqlType: QueryType[Value]
    override def toString = "${"+sqlType.display(value)+"}"
  }

  trait QueryType[A] extends TermExtract[Row, A] {
    def inject: (Statement, Int, A) => Unit
    def display: A => String
  }

  implicit class QueryContext( sc: StringContext) {
    def ql( ps: QueryValue* ): Query = new Query {
      val parts = sc.parts
      val params = ps
    }
  }

  implicit class QueryCapture[A](a: A)(implicit t: QueryType[A]) extends QueryValue {
    type Value = A
    val value = a
    val sqlType = t
  }

  implicit class QueryofValue[A](a: A)(implicit t: QueryType[A]) extends Query {
    val parts = Seq("", "")
    val params = Seq(new QueryCapture(a))
  }

  implicit class QueryofSymbol(s: Symbol) extends Query {
    val parts = Seq("\""+s.name+"\"")
    val params = Seq()
  }

  implicit class QueryofTerm(t: Term) extends Query {
    val parts = Seq("\""+t.name+"\"")
    val params = Seq()
  }

  implicit class RowOps(rs: Row) {
    def get[A](c: Symbol)( implicit e: QueryType[A]): Option[A] = e.tryExtract(rs, c.name)
    def apply[A](c: Symbol)( implicit e: QueryType[A]): A = e.extract(rs, c.name)
    def get(t: Term)(implicit e: QueryType[t.Value]): Option[t.Value] = e.tryExtract(rs, t.name)
    def apply(t: Term)(implicit e: QueryType[t.Value]): t.Value = e.extract(rs, t.name)
  }

  trait TermExtract[R, A] {
    def extract: (R, String) => A
    def tryExtract: (R, String) => Option[A]
  }

  abstract class Term {
    type Value
    def name: String
    def apply()(implicit r: Row, e: TermExtract[Row, Value]): Value = e.extract(r, name)
    def unapply[R](r: R)(implicit e: TermExtract[R, Value]): Option[Value] = e.tryExtract(r, name)
    override def toString = s"Term($name)"
  }

  def term[A](n: String) = new Term {
    type Value = A
    val name = n
  }

  def term[A](s: Symbol): Term { type Value = A } = term(s.name)

  type Context[+A]

  trait Action[A] {
    def run(implicit s: Session): Context[A]
    def flatMap[B](f: A => Action[B]): Action[B]
    def map[B](f: A => B): Action[B]
    def zip[B]( other: Action[B]):Action[(A, B)]
    def >>=[B](f: A => Action[B]): Action[B] = flatMap(f)
    def >>[B](b: Action[B]): Action[B] = flatMap(_ => b)
  }

  def action(q: Query): Action[Int]
  def action[S](q: Query, f: Reducer[Row, S]): Action[S]
}
