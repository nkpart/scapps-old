package com.scapps

import scalaz.Scalaz._
import scalaz.NonEmptyList
import scalaz.OptionW
import slinky.http.request.{POST, Method, Request, GET}
import slinky.http.servlet.{HttpServlet, HttpServletRequest, ServletApplication, StreamStreamServletApplication}
import slinky.http.servlet.HttpServlet._
import slinky.http.StreamStreamApplication._
import slinky.http.{Application, ContentType}
import slinky.http.response.xhtml.Doctype.{transitional, strict}
import slinky.http.response._
import xhtml.Doctype
import xml.Elem;
import scalaz.CharSet._

import com.scapps._
import com.scapps.experimental.OptionKleisli._

import scalaz.Kleisli

import com.scapps.experimental.Routing._
import com.scapps.experimental.OptionKleisli._

class MyApps extends DropApp {
  import t.imps._
  import DropApp._

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
        Some(r.render(view("Beer view", <div>You asked for{r.captures('id)}</div>)))
      }),
      Route(GET, "beers", r => {
        Some(r.render(view("Beer list", <div>You asked for NOTHING!</div>)))
      })
      )
  }

  object TheLols extends DropApp {
    val routes = List(
      Route(GET, "lols", r => {
        Some(r.render(view("Wat", "Scinatra? I barely knows ya!")))
      })
      )
  }

  val routes: List[Route] = DropApp.mount("private", KGoApp) | TheLols
}


