package it.xaan.scalalin.rest

import io.javalin.http.Context

import scala.util.Try

object RouteCheck {
  type Check[T] = (Context, T) => Try[Unit]
}
