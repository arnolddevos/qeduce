package qeduce

import java.sql.ResultSet

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
  }

  object HMap {
    def apply(): HMap = new HMap {
      protected def underlying = Map.empty
    }
  }

  class MutableRow(rs: ResultSet) extends Row {
    def apply[A](c: Symbol)( implicit t: SQLType[A]): A = t.extract(rs, c.name)
    def apply(t: SQLTerm): t.Value = t.sqlType.extract(rs, t.name)
    def project( ts: SQLTerm* ): HMap =
      ts.foldLeft(HMap())((m, t) => m.add(t)(t.sqlType.extract(rs, t.name)))
  }  
}
