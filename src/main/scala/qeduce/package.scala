import anodyne.{HMaps, Rules}

trait HMapRules extends Rules with HMaps {
  type Corpus = HMap
  def addResult(h: HMap, t: Term)(v: t.Value) = h.add(t)(v)
  def removeResult(h: HMap, t: Term) = h.remove(t)
}

package object qeduce
  extends Qeduce
  with SQLTypes
  with Constructions
  with Effects
  with HMaps
  with HMapRules
