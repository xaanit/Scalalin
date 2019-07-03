package it.xaan.scalalin.util

import java.util

object Util {
  def toJavaMap[K](map: Map[K, Any]): java.util.Map[K, Any] = {
    val newMap = new util.HashMap[K, Any]()
    map.foreachEntry { (k, v) =>
      v match {
        case nested: Map[K, Any] => newMap.put(k, toJavaMap(nested))
        case _ => newMap.put(k, v)
      }
    }
    newMap
  }
}
