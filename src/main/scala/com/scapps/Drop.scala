package com.scapps

import scalaz.control._
import scalaz.list.NonEmptyList
import scalaz.OptionW
import slinky.http.request.{POST, Method, Request, GET}
import slinky.http.servlet.{HttpServlet, HttpServletRequest, ServletApplication, StreamStreamServletApplication}
import slinky.http.servlet.HttpServlet._
import slinky.http.StreamStreamApplication._
import slinky.http.{Application, ContentType}
import slinky.http.response.xhtml.Doctype.{transitional, strict}
import slinky.http.response._
import xhtml.Doctype;
import scalaz.CharSet._
import scala.xml.{NodeSeq, Elem}

import com.scapps._

import scalaz.control.Kleisli

import com.scapps.experimental.Routing._
import com.scapps.experimental.OptionKleisli._

object Renderer {
  implicit val charSet = UTF8

  def render(content: Elem)(implicit r: Request[Stream]) = {
    OK(ContentType, "text/html") << transitional << content
  }

  def fourOhFour(implicit r: Request[Stream]) = {
    NotFound(ContentType, "text/html") << transitional << "404 - Not found"
  }

  def respondWith(ct : String, content : String)(implicit r: Request[Stream]) = {
    OK(ContentType, ct) << content
  }
}

abstract case class RoutePart()
case class Dir(s: String) extends RoutePart()
case class Any(name: Symbol) extends RoutePart()

case class RouteLol(parts: List[RoutePart]) {
  def /(r: RoutePart) = {
    RouteLol(parts + r)
  }
}

object t {
  import scalaz.OptionW._

  object imps {
    implicit def RouteLolToListRoute(routelol: RouteLol) = routelol.parts

    implicit def ListToRouteLol(parts: List[RoutePart]) = RouteLol(parts)

    implicit def RouteToRouteLol(rp: RoutePart) = RouteLol(List(rp))

    implicit def StringToRouteLol(rp: String) = RouteLol(List(Dir(rp)))

    implicit def SymbolToRouteLol(rp: Symbol) = RouteLol(List(Any(rp)))

    implicit def SymbolToPart(s: Symbol) = Any(s)

    implicit def StringToPart(s: String) = Dir(s)

    implicit def S2LRP(s: String): List[RoutePart] = List(Dir(s))
  }

  def pathBits(path: String) = {
    val parts = path.split("/").toList.filter(!_.isEmpty)
    parts match {
      case List() => List("")
      case x => x
    }
  }

  def f(part: String, routePart: RoutePart) = routePart match {
    case Dir(s) => cond(part.equals(s), None) //Some of None, if we found something, else None
    case Any(n) => Some(Some(n -> part))
  }

  def matchRoute(path: String)(parts: List[RoutePart]): Option[Map[Symbol, String]] = {
    val x = pathBits(path)
    cond(x.length == parts.length, x) flatMap (bits => {
      val checked = bits.zip(parts) map Function.tupled(f)
      if (checked.forall(_.isDefined)) {
        val l: List[(Symbol, String)] = OptionW.somes(OptionW.somes(checked))
        Some(Map.empty[Symbol, String] ++ l)
      } else {
        None
      }
    })
  }
}
// "lists" / 'id
case class ScappsRequest(captures: Map[Symbol, String], request: Request[Stream]) {
  implicit val r = request

  def render(a: Elem) = Renderer.render(a)

  def respondWith(ct : String, content : String) = Renderer.respondWith(ct, content)
}

case class Route(m: Method, parts: List[RoutePart], f: (ScappsRequest => Option[Response[Stream]]))


trait DropApp {
  val routes: List[Route]

  def |(d: DropApp) = {
    val n = routes ++ d.routes
    new DropApp {val routes = n}
  }
}

object DropApp {
  implicit def DropAppList(d : DropApp) = d.routes
  def mount(base: String, lols: DropApp): DropApp = {
    new DropApp {
      val routes = (lols.routes map (r => Route(r.m, Dir(base) :: r.parts, r.f)))
    }
  }
}