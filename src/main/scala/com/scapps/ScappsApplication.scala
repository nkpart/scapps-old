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
import com.scapps.Routing._
import com.scapps.OptionKleisli._

import scalaz.control.Kleisli

import com.scapps.Routing._
import com.scapps.OptionKleisli._
import slinky.http.request.Request

final class ScappsApplication extends StreamStreamServletApplication {
  object App {
    implicit val charSet = UTF8
    import scalaz.OptionW.onull
    import scalaz.javas.Iterator._

    def respond(dropApp: DropApp)(implicit request: Request[Stream]) = {
      def f(route: Route)(request: Request[Stream]): Option[Response[Stream]] = {
        t.matchRoute(request.path.mkString)(route.parts) flatMap (m => route.f(ScappsRequest(m, request)))
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
        val p = "com.scapps.MyApps" // TODO
        if (p == null) {
          throw new RuntimeException("Set scapps app class plz.")
        }
        val c = Class.forName(p)
        if (classOf[DropApp] isAssignableFrom c) {
          a = c.newInstance.asInstanceOf[DropApp]
          //servlet.servlet.getServletConfig.get
        } else {
          throw new RuntimeException("Bad class.")
        }
      }

      // loads war resource if app doesn't respond to request
      //quicklists getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
      respond(a) getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
    }
  }
}
