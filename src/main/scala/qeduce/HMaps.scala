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
    def keys: Iterable[SQLTerm] = underlying.keys
  }

  object HMap {
    def apply(): HMap = new HMap {
      protected def underlying = Map.empty
    }

    def apply( row: Row, ts: SQLTerm* ): HMap = 
      ts.foldLeft(HMap())((m, t) => m.add(t)(row(t)))
  }
}
