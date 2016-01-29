package qeduce

trait SQLTypes { this: Qeduce =>
  
  import scala.language.implicitConversions

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
}
