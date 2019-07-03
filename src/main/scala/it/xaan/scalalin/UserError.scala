package it.xaan.scalalin

class UserError(
                 val code: Int,
                 val json: Map[String, Any] = Map(),
                 val headers: Map[String, String] = Map()
               ) extends RuntimeException("Users Error")