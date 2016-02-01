import qeduce._
import java.sql.{Connection, SQLException, ResultSet, PreparedStatement}
import transducers._

object Examples extends App { 

  def test1(rs: ResultSet) = {
    val i: Int = rs -> 'i
  }

  def test2(x: Int) = {
    sql"select * from t1 where a < $x and b = ${4.0}"
  }


  println(test2(12))
  println(test2(12).parts)
  println(test2(12).params)
  println('table ~ list( 'x, 12 ))
  println('table ~ nest( 'x, 12 ))
  def pr(s: SQL) = println(s.parts, s.params)
  pr('table ~ sql"(" ~ 12)
  println('table ~ sql"(" ~ 12)
}
