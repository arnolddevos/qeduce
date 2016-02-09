package qeduce

import transducers.{Transducer, map}

trait Rules { this: Qeduce with HMaps =>

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

    val rs0 = rs.filter(r => h.get(r.head).isEmpty).reverse
    iterate(h, rs0, Nil, false)
  }

  def project(ts: SQLTerm*): Transducer[HMap, Row] = map(HMap(_, ts:_*))
  def infer(rs: List[Rule]): Transducer[HMap, HMap] = map(applyRules(_, rs))
}
