package it.xaan.scalalin

import io.javalin.Javalin
import io.javalin.apibuilder.ApiBuilder._
import io.javalin.apibuilder.EndpointGroup
import io.javalin.http.Context
import play.api.libs.json.{JsNull, JsObject, Json}

import scala.util.{Failure, Success, Try}

abstract class Route(val path: String, val app: Javalin) {
  def call(ctx: Context): Unit = {
    before(ctx) match {
      case Failure(exception) =>
        exception match {
          case ex: HttpException => json(json = ex.json, code = ex.code)(ctx)
          case _ => json(Json.obj("error" -> exception.getMessage), code = 500)(ctx)
        }
      case Success(_) =>
        val call = Method.of(ctx.method()) match {
          case Get => get(ctx)
          case Put => put(ctx)
          case Head => head(ctx)
          case Post => post(ctx)
          case Patch => patch(ctx)
          case Delete => delete(ctx)
          case Unknown => unknown(ctx)
        }
        call.failed.foreach(exception => json(Json.obj("error" -> exception.getMessage), code = 500)(ctx))
    }
  }

  private def _405(implicit ctx: Context) = json(Json.obj("error" -> "Method not allowed."), code = 405)

  def before(implicit ctx: Context): Try[Boolean]

  def get(implicit ctx: Context): Try[Unit] = _405

  def put(implicit ctx: Context): Try[Unit] = _405

  def head(implicit ctx: Context): Try[Unit] = _405

  def post(implicit ctx: Context): Try[Unit] = _405

  def patch(implicit ctx: Context): Try[Unit] = _405

  def delete(implicit ctx: Context): Try[Unit] = _405

  def unknown(implicit ctx: Context): Try[Unit] = Failure(new IllegalArgumentException(s"Unknown ctx method: ${ctx.method()}"))

  def json(json: JsObject, code: Int, headers: Map[String, String] = Map())(implicit ctx: Context): Try[Unit] =
    respond(json.toString(), code, Map("Content-Type" -> "application/json") ++ headers)

  def respond(text: String, code: Int, headers: Map[String, String] = Map())(implicit ctx: Context): Try[Unit] =
    Try {
      (Json.obj("what" -> JsNull) \ "what").as[String]
      headers.foreach { case (key, value) => ctx.header(key, value) }
      ctx.status(code)
      ctx.result(text)
    }
}

object Route {
  def getEndpoints[T](routes: Seq[Route]): EndpointGroup =
    () => {
      routes.foreach { route =>
        path(route.path, () => {
          get(route.call)
          post(route.call)
          patch(route.call)
          delete(route.call)
          put(route.call)
          head(route.call)
        })
      }
    }
}
