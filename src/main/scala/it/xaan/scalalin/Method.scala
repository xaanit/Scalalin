package it.xaan.scalalin

trait Method
case object Get extends Method
case object Patch extends Method
case object Delete extends Method
case object Put extends Method
case object Head extends Method
case object Post extends Method
case object Unknown extends Method

case object Method {
  def of(str: String): Method =
    str.toLowerCase match {
    case "get" => Get
    case "patch" => Patch
    case "delete" => Delete
    case "put" => Put
    case "head" => Head
    case "post" => Post
    case _ => Unknown
  }
}