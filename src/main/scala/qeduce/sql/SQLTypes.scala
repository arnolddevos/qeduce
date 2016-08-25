package qeduce.sql

import java.sql
import qeduce.generic.Qeduce

trait SQLTypes { this: Qeduce =>

  type Session = sql.Connection
  type Statement = sql.PreparedStatement
  type Row = sql.ResultSet

  abstract class SQLType[A] extends QueryType[A] {
    def tryExtract =
      (rs, n) =>
        try { Some(extract(rs, n)) }
        catch { case _:sql.SQLException => None }
  }

  implicit object sqlInt extends SQLType[Int] {
    def extract = _ getInt _
    def inject = _.setInt(_, _)
    def display = _.toString
  }

  implicit object sqlLong extends SQLType[Long] {
    def extract = _ getLong _
    def inject = _.setLong(_, _)
    def display = _.toString
  }

  implicit object sqlDouble extends SQLType[Double] {
    def extract = _ getDouble _
    def inject = _.setDouble(_, _)
    def display = _.toString
  }

  implicit object sqlBoolean extends SQLType[Boolean] {
    def extract = _ getBoolean _
    def inject = _.setBoolean(_, _)
    def display = _.toString
  }

  implicit object sqlString extends SQLType[String] {
    def extract = _ getString _
    def inject = _.setString(_, _)
    def display = "\"" + _ + "\""
  }

  implicit def sqlNullable[A]( implicit u: QueryType[A]): QueryType[Option[A]] = 
    new SQLType[Option[A]] {
      def extract = {
        (rs, name) =>
          val a = u.extract(rs, name)
          if(rs.wasNull) None else Some(a)
      }
      def inject = {
        (st, ix, as) =>
          if(as.isDefined) u.inject(st, ix, as.get)
          else st.setObject(ix, null)
      }
      def display =
        as =>
          if(as.isDefined) "Some(" + u.display(as.get) + ")"
          else "None"
    }
}
