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

import com.scapps._
import com.scapps.experimental.Routing._
import com.scapps.experimental.OptionKleisli._

import scalaz.control.Kleisli

import slinky.http.request.Request

final class ScappsApplication extends StreamStreamServletApplication {
  object App {
    implicit val charSet = UTF8
    import scalaz.OptionW.onull
    import scalaz.javas.Iterator._

    def respond(dropApp: DropApp)(implicit request: Request[Stream]) = {
      def f(route: Route)(request: Request[Stream]): Option[Response[Stream]] = {
        val matchMethod = OptionW.cond(route.m.equals(request.method), request)
        val matchRoute = matchMethod flatMap (r => t.matchRoute(r.path.mkString)(route.parts))
        matchRoute flatMap (m => route.f(ScappsRequest(m, request)))
      }
      val x: List[Kleisli[Option, Request[Stream], Response[Stream]]] = dropApp.routes map (f(_) _)
      x(request)
    }
  }

  import App._

  var a : DropApp = null

  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      if (a == null) {
        val appName = servlet.servlet.getInitParameter("scapps-application")
        if (appName == null) {
          // TODO: make this really clear
          throw new RuntimeException("Set scapps app class plz.")
        }
        val c = Class.forName(appName)
        if (classOf[DropApp] isAssignableFrom c) {
          a = c.newInstance.asInstanceOf[DropApp]
          //servlet.servlet.getServletConfig.get
        } else {
          throw new RuntimeException("Specified app class is not assignable from DropApp.")
        }
      }
      respond(a) getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
    }
  }
}
