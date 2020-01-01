package it.xaan.scalalin

import play.api.libs.json.{JsObject, Json}

class HttpException(
                 val code: Int = 400,
                 val json: JsObject = Json.obj("error" -> "Invalid request"),
                 val headers: Map[String, String] = Map()
               ) extends RuntimeException("Users Error")