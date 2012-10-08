package jawn

import scala.collection.mutable

sealed trait Value { def j: String }

case object True extends Value { final def j = "true" }
case object False extends Value { final def j = "false" }
case object Null extends Value { final def j = "null" }

case class Str(s: String) extends Value {
  def j = "\"%s\"" format Str.escape(s)
}

object Str {
  def escape(s: String) = s.replace("\\", "\\\\").replace("\"", "\\\"")
}

case class LongNum(n: Long) extends Value { def j = n.toString }
case class DoubleNum(n: Double) extends Value { def j = n.toString }
case class DeferNum(s: String) extends Value { def j = s } //lol

sealed trait Container extends Value

case class Arr(vs: mutable.ArrayBuffer[Value]) extends Container {
  def j = if (vs.length == 0) {
    "[]"
  } else {
    val sb = new StringBuilder().append("[")
    vs.foreach(v => sb.append(v.j).append(", "))
    sb.dropRight(2).append("]").toString
  }
}

case class Obj(vs: mutable.Map[String, Value]) extends Container {
  def j = if (vs.size == 0) {
    "{}"
  } else {
    val sb = new StringBuilder().append("[")
    vs.foreach {
      case (k, v) => sb.append("\"" + k + "\": ").append(v.j).append(", ")
    }
    sb.dropRight(2).append("}").toString
  }
}
