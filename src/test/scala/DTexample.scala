object DT {

  trait Key { type Value }

  class Setting(val str: String) extends Key
  val sort = new Setting("sort") { type Value = String }
  val width = new Setting("width") { type Value = Int }

  val params = HMap.empty.add(width)(120).add(sort)("time")

  val x: Option[Int] = params.get(width)
  val y: Option[String] = params.get(sort)

  trait HMap { self =>
    def get(key: Key): Option[key.Value]
    def add(key: Key)(value: key.Value) = new HMap {
      def get(k: Key) =
        if (k == key) Some(value.asInstanceOf[k.Value])
        else self.get(k)
    }
  }

  object HMap {
    def empty = new HMap { def get(k: Key) = None }
  }  
}

object TP {

  trait Key[Value] {}

  class Setting[Value](val str: String) extends Key[Value]
  val sort = new Setting[String]("sort")
  val width = new Setting[Int]("width")

  val params = HMap.empty.add(width)(120).add(sort)("time")

  val x: Option[Int] = params.get(width)
  val y: Option[String] = params.get(sort)

  trait HMap { self =>
    def get[Value](key: Key[Value]): Option[Value]
    def add[Value](key: Key[Value])(value: Value) = new HMap {
      def get[X](k: Key[X]) =
        if (k == key) Some(value.asInstanceOf[X])
        else self.get(k)
    }
  }

  object HMap {
    def empty = new HMap { def get[Value](k: Key[Value]) = None }
  }  
}

object Run extends App {
  println(s"DT: x=${DT.x} y=${DT.y}")
  println(s"TP: x=${TP.x} y=${TP.y}")
}

