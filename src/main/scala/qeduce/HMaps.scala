package qeduce

import java.sql.{ResultSet, SQLException}

trait HMaps { this: Qeduce =>

  trait HMap extends Row { self =>
    protected def underlying: Map[SQLTerm, Any]
    def get(t: SQLTerm): Option[t.Value] = underlying.get(t).asInstanceOf[Option[t.Value]] 
    def apply(t: SQLTerm): t.Value = underlying(t).asInstanceOf[t.Value]
    def add(t: SQLTerm)( a: t.Value): HMap = new HMap { 
      val underlying = self.underlying.updated(t, a) 
    }
    def project( ts: SQLTerm* ): HMap = 
      ts.foldLeft(HMap())((m, t) => m.add(t)(self(t)))
    def keys: Iterable[SQLTerm] = underlying.keys
  }

  object HMap {
    def apply(): HMap = new HMap {
      protected def underlying = Map.empty
    }
  }

  class MutableRow(rs: ResultSet) extends Row {
    def get(t: SQLTerm): Option[t.Value] = 
      try { Some(apply(t))} catch { case _:SQLException => None }
    def apply[A](c: Symbol)( implicit t: SQLType[A]): A = t.extract(rs, c.name)
    def apply(t: SQLTerm): t.Value = t.sqlType.extract(rs, t.name)
    def project( ts: SQLTerm* ): HMap =
      ts.foldLeft(HMap())((m, t) => m.add(t)(t.sqlType.extract(rs, t.name)))
  }  

  trait Rule { 
    val head: SQLTerm
    val body: HMap => Option[head.Value]
    override def toString = s"rule($head)"
  }

  def rulePf(h: SQLTerm)( b: PartialFunction[HMap, h.Value]): Rule = 
    ruleOf(h)(b.lift)

  def ruleOf(h: SQLTerm)( b: HMap => Option[h.Value]): Rule = {
    new Rule {
      val head = h: SQLTerm { type Value = h.Value }
      val body = b
    }
  }

  def applyRules(h: HMap, rs: List[Rule]): HMap = {

    @annotation.tailrec
    def iterate(h: HMap, rs0: List[Rule], rs1: List[Rule], fired: Boolean): HMap =
      rs0 match {
        case r :: rs => 
          r.body(h) match {
            case Some(a) => iterate(h.add(r.head)(a), rs, rs1, true)
            case None => iterate(h, rs, r :: rs1, fired)
          }

        case Nil => 
          if(fired) iterate(h, rs1.reverse, Nil, false)
          else h
      }

    val rs0 = rs.filter(r => h.get(r.head).isEmpty)
    iterate(h, rs0, Nil, false)
  }
}
