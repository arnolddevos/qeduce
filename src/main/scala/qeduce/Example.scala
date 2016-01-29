import qeduce._
import java.sql.{Connection, SQLException, ResultSet, PreparedStatement}
import transducers._

object Examples extends App { 

  def test1(rs: ResultSet) = {
    val i: Int = rs -> 'i
  }

  def test2(x: Int) = {
    sql"select * from t1 where a < $x"
  }

  def test3 = {
    'table ( 'x, 12 )
  }

  println(test2(12))
  println(test2(12).parts)
  println(test2(12).params)
  println(test3)
  def pr(s: SQL) = println(s.parts, s.params)
  pr('table ~ sql"(" ~ 12)
  println('table ~ sql"(" ~ 12)
}
