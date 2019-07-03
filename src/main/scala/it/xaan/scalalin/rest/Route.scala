package it.xaan.scalalin.rest

import io.javalin.apibuilder.EndpointGroup
import io.javalin.http.Context
import it.xaan.scalalin.UserError
import it.xaan.scalalin.rest.RouteCheck.Check
import it.xaan.scalalin.util.Util
import io.javalin.apibuilder.ApiBuilder._

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
          case ue: UserError => respond(ue.code, ue.json, ue.headers)(ctx)
          case ex: Exception => respond(code = 500, json = Map("error" -> "Internal error."))(ctx)
            ex.printStackTrace()
        }
      case Success(_) => ctx.method().toUpperCase() match {
        case "GET" => get(ctx)
        case "POST" => post(ctx)
        case "HEAD" => head(ctx)
        case "PUT" => put(ctx)
        case "DELETE" => delete(ctx)
        case "PATCH" => patch(ctx)
        case _ =>
      }
    }

  final def head(ctx: Context): Unit = respond(200, Map())(ctx)

  def get(ctx: Context): Unit = respond(405, Map("error" -> "Method not allowed"))(ctx)

  def post(ctx: Context): Unit = respond(405, Map("error" -> "Method not allowed"))(ctx)

  def patch(ctx: Context): Unit = respond(405, Map("error" -> "Method not allowed"))(ctx)

  def delete(ctx: Context): Unit = respond(405, Map("error" -> "Method not allowed"))(ctx)

  def put(ctx: Context): Unit = respond(405, Map("error" -> "Method not allowed"))(ctx)


  def respond(code: Int, json: Map[String, Any], headers: Map[String, String] = Map())(implicit ctx: Context): Unit = {
    ctx.status(code)
      .json(Util.toJavaMap(json))
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
