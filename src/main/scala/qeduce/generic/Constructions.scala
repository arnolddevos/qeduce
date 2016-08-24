package qeduce.generic

trait Constructions { this: Qeduce =>

  val select = sql"select"
  val insert = sql"insert"
  val update = sql"update"
  val from   = sql"from"
  val where  = sql"where"
  val as     = sql"as"
  val values = sql"values"
  val set    = sql"set"
  val into   = sql"into"
  val order  = sql"order"
  val group  = sql"group"
  val by     = sql"by"
  val in     = sql"in"

  def list(items: SQL*): SQL =
   if(items.nonEmpty) items.reduce( _ ~ sql"," ~ _)
   else sql""

  def nest(items: SQL*): SQL =
    sql"(" ~ list(items: _*) ~ sql")"

  def qualify( scope: SQL)( members: SQL*): SQL =
    list(members map (scope ~ sql"." ~ _): _*)

}
