import anodyne.{HMaps, Rules}
import qeduce.generic._
import qeduce.sql._

package object qeduce
  extends Qeduce
  with SQLTypes
  with SQLActions
  with Constructions
  with HMaps
  with Rules
