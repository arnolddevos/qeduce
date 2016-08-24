import anodyne.{HMaps, Rules}
import qeduce.generic._
import qeduce.sql._

package object qeduce
  extends Qeduce
  with SQLTypes
  with Constructions
  with Effects
  with HMaps
  with Rules
