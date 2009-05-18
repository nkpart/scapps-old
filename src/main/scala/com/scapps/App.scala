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

object MyApps extends DropApp {
  import t.imps._
  import DropApp._
  import scalaz.OptionW.onull

  def view[A](title: String, content: A) =
    <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
    <title>{title}</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width = 320px"/>
    <link rel="stylesheet" href="/style.css" media="screen"/>
    </head>
    <body>{content}</body>
    </html>


  object KGoApp extends DropApp {
    val routes = List(
      Route(GET, "beers" / 'id, r => {
        r.render(view("Beer view", <div>You asked for{r.captures('id)}</div>))
      }),
      Route(GET, "beers", r => {
        r.render(view("Beer list", <div>You asked for NOTHING!</div>))
      })
      )
  }

  object TheLols extends DropApp {
    val routes = List(
      Route(GET, "lols", r => r.render(view("Wat", "Scinatra? I barely knows ya!")))
      )
  }

  val routes: List[Route] = DropApp.mount("private", KGoApp) | TheLols
}


final class App extends StreamStreamServletApplication {
  object App {
    implicit val charSet = UTF8
    import scalaz.OptionW.onull
    import scalaz.javas.Iterator._

    def drop(implicit request: Request[Stream], servletRequest: HttpServletRequest): Option[Response[Stream]] = {
      respond(MyApps)
    }

    def respond(dropApp: DropApp)(implicit request: Request[Stream]) = {
      def f(route: Route)(request: Request[Stream]): Option[Response[Stream]] = {
        t.matchRoute(request.path.mkString)(route.parts) flatMap (m => route.f(ScappsRequest(m, request)))
      }
      val x: List[Kleisli[Option, Request[Stream], Response[Stream]]] = dropApp.routes map (f(_) _)
      x(request)
    }
  }
  import App._

  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      // loads war resource if app doesn't respond to request
      //quicklists getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
      drop getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
    }
  }
}


