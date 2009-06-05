package com.scapps.experimental

import scalaz.Kleisli
import scalaz.{OptionW, StringW}
import slinky.http.request._
import slinky.http.response.Response
import scalaz.NonEmptyList._
import scalaz.NonEmptyList
import scalaz.OptionW._
import scalaz.StringW._
import scalaz.Monad._
import scalaz.Scalaz._

object OptionKleisli {
  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](f)  
}

object Routing {
  //todo put this in scalaz
  implicit def NonEmptyListList[T](nel : NonEmptyList[T]) : List[T] = nel.list
  implicit def ListOptionNonEmptyList[T](list : List[T]) : Option[NonEmptyList[T]] = list match {
    case Nil => None
    case a :: as => Some(nel(a, as))
  }
  
  
  def firstSome[A, B](fs: List[Kleisli[Option, A, B]])(a: A): Option[B] = (fs.elements.map(_(a)).find(_.isDefined)).join

  implicit def ListKleisliKleisli[A, B](fs: List[Kleisli[Option, A, B]]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](firstSome(fs) _)

  implicit def PathKliesli(s: String) = Kleisli.kleisli[Option](exactdir(s))

  implicit def MethodKliesli(m: Method) = Kleisli.kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  def stringToNel(s : String, n : NonEmptyList[Char]) = {
    if (s.length == 0) n else {
      val l = s.toList
      nel(l.head, l.tail)
    }
  }
  // returns a new request with the prefix stripped if it matches, else none
  def dir(prefix: String) = {
    def f(prefix: String)(r: Request[Stream]): Option[Request[Stream]] = {
      (r.pathStartsWith(prefix)).option(
        r(r.uri(stringToNel(r.path.list.mkString.replaceFirst(prefix, ""), NonEmptyList.nel('/'))))
      )
    }
    f(prefix) _
  }


  // returns a new request with the uri stripped to '/', if it matches exactly, else none
  def exactdir(s: String) : (Request[Stream] => Option[Request[Stream]]) = {
    def g(s: String)(r: Request[Stream]): Option[Request[Stream]] = (r.pathEquals(s)).option(r(r.uri(NonEmptyList.nel('/'))))
    g(s) _
  }

  def withParam(key: String) = (r: Request[Stream]) => (r !? key).option(r)

  def methodM(m: Method) : (Request[Stream] => Option[Request[Stream]]) = (r: Request[Stream]) => { (r.method.equals(m)).option(r) }

  // interprets the value of _method in a post as an HTTP method
  def methodHax = (r: Request[Stream]) => {
    val mbMeth: Option[Method] = (r | "_method") |> (_.mkString) >>= (Method.StringMethod(_))
    Some((mbMeth |> (r(_))) getOrElse (r))
  }
}

import Routing.{ListOptionNonEmptyList,NonEmptyListList}

case class ItemRequest(request: Request[Stream], key: NonEmptyList[Char]) {
  import com.scapps.StringParser._
  lazy val longKey: Option[Long] = key.mkString.asLong
}

/*
/beers/1[/]
/beers/2/reviews[/]
 */
object ItemRequest {  
  def RequestOptionItemRequest(request: Request[Stream]): Option[ItemRequest] = {
    val sid: Option[NonEmptyList[Char]] = request.path.mkString.split("/")(0).toList
    (sid |> (ItemRequest(request, _)))
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