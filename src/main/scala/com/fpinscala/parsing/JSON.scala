package com.fpinscala.parsing

/**
 * Created by elkorn on 1/11/15.
 */
trait JSON

object JSON {

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBoolean(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  case object JNull extends JSON

}
