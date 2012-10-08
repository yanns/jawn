package jawn

import scala.collection.mutable

sealed trait Context {
  def add(s: String): Unit
  def add(v: Value): Unit
  def finish: Container
  def isObj: Boolean
}

final class ArrContext extends Context {
  private val vs = mutable.ArrayBuffer.empty[Value]

  def add(s: String): Unit = vs.append(Str(s))
  def add(v: Value): Unit = vs.append(v)
  def finish: Arr = new Arr(vs)
  def isObj = false
}

final class ObjContext extends Context {
  private var key: String = null
  private val vs = mutable.Map.empty[String, Value]

  def add(s: String): Unit = if (key == null) {
    key = s
  } else {
    vs(key) = Str(s)
    key = null
  }

  def add(v: Value): Unit = { vs(key) = v; key = null }

  def finish: Obj = new Obj(vs)
  def isObj = true
}
