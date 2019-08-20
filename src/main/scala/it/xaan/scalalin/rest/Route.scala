package it.xaan.scalalin.rest

import io.javalin.apibuilder.ApiBuilder._
import io.javalin.apibuilder.EndpointGroup
import io.javalin.http.Context
import io.javalin.plugin.json.JavalinJson
import it.xaan.scalalin.UserError
import it.xaan.scalalin.rest.RouteCheck.Check
import it.xaan.scalalin.util.Util

import scala.util.{Failure, Success, Try}

abstract class Route[T](
                         val path: String,
                         val checks: Seq[Check[T]] = Seq(),
                         val custom: T = null
                       ) {
  def validate(ctx: Context): Try[Unit] =
    checks.map(_ (ctx, custom)).collectFirst { case r@Failure(_) => r }.getOrElse(Success())

  def call(ctx: Context): Unit =
    validate(ctx) match {
      case Failure(exception) =>
        exception match {
          case ue: UserError => respondMap(ue.code, ue.json, ue.headers)(ctx)
          case ex: Exception => respondMap(code = 500, json = Map("error" -> "Internal error."))(ctx)
            ex.printStackTrace()
        }
      case Success(_) => Try(ctx.method().toUpperCase() match {
        case "GET" => get(ctx)
        case "POST" => post(ctx)
        case "HEAD" => head(ctx)
        case "PUT" => put(ctx)
        case "DELETE" => delete(ctx)
        case "PATCH" => patch(ctx)
        case _ =>
      }) match {
        case Failure(exception) =>
          exception match {
            case ue: UserError => respondMap(ue.code, ue.json, ue.headers)(ctx)
            case ex: Exception => respondMap(code = 500, json = Map("error" -> "Internal error."))(ctx)
              ex.printStackTrace()
          }
        case Success(_) => //ignore, we're cool
      }
    }

  final def head(implicit ctx: Context): Unit = respondMap(200, Map())(ctx)

  def get(implicit ctx: Context): Unit = respondMap(405, Map("error" -> "Method not allowed"))(ctx)

  def post(implicit ctx: Context): Unit = respondMap(405, Map("error" -> "Method not allowed"))(ctx)

  def patch(implicit ctx: Context): Unit = respondMap(405, Map("error" -> "Method not allowed"))(ctx)

  def delete(implicit ctx: Context): Unit = respondMap(405, Map("error" -> "Method not allowed"))(ctx)

  def put(implicit ctx: Context): Unit = respondMap(405, Map("error" -> "Method not allowed"))(ctx)

  def respondMap(code: Int, json: Map[String, Any], headers: Map[String, String] = Map())(implicit ctx: Context): Unit = respond(code, Util.toJavaMap(json), headers)

  def respond(code: Int, json: Any, headers: Map[String, String] = Map())(implicit ctx: Context): Unit = {
    ctx.status(code)
      .result(json match {
        case str: String => str
        case _ => JavalinJson.toJson(json)
      })
      .header("Content-Type", "application/json")
    headers.foreachEntry { (key, value) => ctx.header(key, value) }
  }

  def getHeader(name: String)(implicit ctx: Context): String = try ctx.header(name) catch {
    case ex: Exception => ""
  }

  def getQuery(name: String)(implicit ctx: Context): String = try ctx.queryParam(name) catch {
    case ex: Exception => ""
  }

  def path(name: String)(implicit ctx: Context): String = ctx.pathParamMap().getOrDefault(name, "")
}

object Route {
  def getEndpoints[T](routes: Seq[Route[T]]): EndpointGroup =
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
