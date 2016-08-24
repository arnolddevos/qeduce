package qeduce.generic

trait Constructions { this: Qeduce =>

  val select = ql"select"
  val insert = ql"insert"
  val update = ql"update"
  val from   = ql"from"
  val where  = ql"where"
  val as     = ql"as"
  val values = ql"values"
  val set    = ql"set"
  val into   = ql"into"
  val order  = ql"order"
  val group  = ql"group"
  val by     = ql"by"
  val in     = ql"in"

  def list(items: Query*): Query =
   if(items.nonEmpty) items.reduce( _ ~ ql"," ~ _)
   else ql""

  def nest(items: Query*): Query =
    ql"(" ~ list(items: _*) ~ ql")"

  def qualify( scope: Query)( members: Query*): Query =
    list(members map (scope ~ ql"." ~ _): _*)

}
