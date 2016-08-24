package qeduce.sql

import java.sql.{SQLException, PreparedStatement, ResultSet}
import qeduce.generic.Qeduce

trait SQLTypes { this: Qeduce =>

  type Statement = PreparedStatement
  type Row = ResultSet

  abstract class SQLBaseType[A] extends SQLType[A] {
    def tryExtract =
      (rs, n) =>
        try { Some(extract(rs, n))} catch { case _:SQLException => None }
  }

  implicit object sqlInt extends SQLBaseType[Int] {
    def extract = _ getInt _
    def inject = _.setInt(_, _)
    def display = _.toString
  }

  implicit object sqlLong extends SQLBaseType[Long] {
    def extract = _ getLong _
    def inject = _.setLong(_, _)
    def display = _.toString
  }

  implicit object sqlDouble extends SQLBaseType[Double] {
    def extract = _ getDouble _
    def inject = _.setDouble(_, _)
    def display = _.toString
  }

  implicit object sqlBoolean extends SQLBaseType[Boolean] {
    def extract = _ getBoolean _
    def inject = _.setBoolean(_, _)
    def display = _.toString
  }

  implicit object sqlString extends SQLBaseType[String] {
    def extract = _ getString _
    def inject = _.setString(_, _)
    def display = "\"" + _ + "\""
  }

  implicit def sqlNullable[A]( implicit u: SQLType[A]): SQLType[Option[A]] = new SQLBaseType[Option[A]] {
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
