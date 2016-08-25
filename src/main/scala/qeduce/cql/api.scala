package qeduce.cql

import anodyne.{HMaps, Rules}
import qeduce.generic._

object api
  extends Qeduce
  with CQLTypes
  with CQLActions
  with Constructions
  with HMaps
  with Rules
