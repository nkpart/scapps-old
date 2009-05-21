package com.scapps.experimental

import scalaz.control.Kleisli
import scalaz.{OptionW, StringW}
import slinky.http.request._
import slinky.http.response.Response
import scalaz.list.NonEmptyList._
import scalaz.list.NonEmptyList
import scalaz.OptionW._
import scalaz.StringW._
import scalaz.control.MonadW.{EitherMonad, OptionMonad, EitherLeftMonad, ListMonad}

object OptionKleisli {
  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](f)  
}

object Routing {
  def firstSome[A, B](fs: List[Kleisli[Option, A, B]])(a: A): Option[B] = join(fs.elements.map(_(a)).find(_.isDefined))

  implicit def ListKleisliKleisli[A, B](fs: List[Kleisli[Option, A, B]]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](firstSome(fs) _)

  implicit def PathKliesli(s: String) = Kleisli.kleisli[Option](exactdir(s))

  implicit def MethodKliesli(m: Method) = Kleisli.kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  // returns a new request with the prefix stripped if it matches, else none
  def dir(prefix: String) = {
    def f(prefix: String)(r: Request[Stream]): Option[Request[Stream]] = {
      OptionW.cond(r.pathStartsWith(prefix), r(r.uri(string(r.path.replaceFirst(prefix, ""), NonEmptyList('/')))))
    }
    f(prefix) _
  }


  // returns a new request with the uri stripped to '/', if it matches exactly, else none
  def exactdir(s: String) = {
    def g(s: String)(r: Request[Stream]): Option[Request[Stream]] = cond(r.pathEquals(s), r(r.uri(NonEmptyList('/'))))
    g(s) _
  }

  def withParam(key: String) = (r: Request[Stream]) => cond(r !? key, r)

  def methodM(m: Method) = (r: Request[Stream]) => cond(r.method.equals(m), r)

  // interprets the value of _method in a post as an HTTP method
  def methodHax = (r: Request[Stream]) => {
    val mbMeth: Option[Method] = (r | "_method") > (_.mkString) >>= (Method.StringMethod(_))
    Some((mbMeth > (r(_))) getOrElse (r))
  }

}

case class ItemRequest(request: Request[Stream], key: NonEmptyList[Char]) {
  lazy val longKey: Option[Long] = key.mkString.parseLong.right.toOption
}

case class RestfulRequest(top : Pair[String, Option[String]], context : List[Pair[String, String]], request : Request[Stream])

object RestfulRequest {
  private def pairUp[T](list : List[T]) : List[Pair[T, Option[T]]] = {
    val cs: List[List[T]] = list.foldLeft(List[List[T]]())((l, x) => l match {
      case Nil => List(x) :: Nil
      case (List(a) :: tail) => List(a, x) :: tail
      case b => List(x) :: b
    })

    val xs = cs map (s => s match {
      case List(a) => Pair(a, None)
      case List(a, b) => Pair(a, Some(b))
    })
    xs
  }

  def f[T](p : Pair[T, Option[T]]) = p match {
    case (a, Some(b)) => Some(a -> b)
    case _ => None
  }

  def from(request : Request[Stream]) : Option[RestfulRequest] = {
    val path: String = request.path.mkString
    val components : List[String] = path.split("/").toList.filter(!_.isEmpty)
    val ls = pairUp(components)

    val mbTop = ls.headOption
    val list: List[Tuple2[String, Option[String]]] = ls.tail
    if (list.forall(_.isDefined) && mbTop.isDefined) {
      val rest = OptionW.somes(list map f[String])
      Some(RestfulRequest(mbTop.get, rest, request))
    } else {
      None
    }
  }
}

/*
/beers/1[/]
/beers/2/reviews[/]
 */
object ItemRequest {
  def RequestOptionItemRequest(request: Request[Stream]): Option[ItemRequest] = {
    val sid: Option[NonEmptyList[Char]] = StringW.StringWList(request.path.mkString.split("/")(0))
    (sid > (ItemRequest(request, _)))
  }
}

trait RestfulResource {
  import Routing._
  import OptionKleisli.OptionKleisli

  val base: String

  def index(r: Request[Stream]): Option[Response[Stream]]

  def formForCreate(r: Request[Stream]): Option[Response[Stream]]

  def createItem(r: Request[Stream]): Option[Response[Stream]]

  def formForEdit(request: ItemRequest): Option[Response[Stream]]

  def deleteItem(r: ItemRequest): Option[Response[Stream]]

  def updateItem(r: ItemRequest): Option[Response[Stream]]

  def item(r: ItemRequest): Option[Response[Stream]]

  def routes : Kleisli[Option, Request[Stream], Response[Stream]]  = {
    methodHax >=> dir("/" + base) >=> List(
      GET >=> List(
        "/" >=> index _,
        "/new" >=> formForCreate _,
        dir("/") >=> withParam("edit") >=> ItemRequest.RequestOptionItemRequest _ >=> formForEdit _,
        dir("/") >=> ItemRequest.RequestOptionItemRequest _ >=> item _
        ),
      PUT >=> dir("/") >=> ItemRequest.RequestOptionItemRequest _ >=> updateItem _,
      POST >=> "/" >=> createItem _,
      DELETE >=> dir("/") >=> ItemRequest.RequestOptionItemRequest _ >=> deleteItem _
      )
  }
}

class LoggingResource(logF : (String => Unit), resource : RestfulResource) extends RestfulResource {

  val base: String = resource.base

  def index(r: Request[Stream]): Option[Response[Stream]] = {
    logF("index")
    resource.index(r)
  }

  def formForCreate(r: Request[Stream]): Option[Response[Stream]] = {
    logF("formForCreate")
    resource.formForCreate(r)
  }

  def createItem(r: Request[Stream]): Option[Response[Stream]] = {
    logF("createItem")
    resource.createItem(r)
  }

  def formForEdit(r: ItemRequest): Option[Response[Stream]] = {
    logF("formForEdit")
    resource.formForEdit(r)
  }

  def deleteItem(r: ItemRequest): Option[Response[Stream]] = {
    logF("deleteItem")
    resource.deleteItem(r)
  }

  def updateItem(r: ItemRequest): Option[Response[Stream]] = {
    logF("updateItem")
    resource.updateItem(r)
  }

  def item(r: ItemRequest): Option[Response[Stream]] = {
    logF("item")
    resource.item(r)
  }
}