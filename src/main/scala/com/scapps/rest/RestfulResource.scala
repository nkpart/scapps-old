package com.scapps.rest


import scalaz.Scalaz._
import scalaz.{NonEmptyList, Kleisli}
import slinky.http.request._
import slinky.http.response.Response


case class ItemRequest(request: Request[Stream], key: NonEmptyList[Char]) {
  import com.scapps.utils.StringParser._
  lazy val longKey: Option[Long] = key.list.mkString.asLong
}

final class RequestW(val request : Request[Stream]) {
  def asItemRequest : Option[ItemRequest] = {
    val sid: Option[NonEmptyList[Char]] = request.path.list.mkString.split("/")(0).toList.nel
    (sid |> (ItemRequest(request, _)))
  }
}

object RequestW {
  implicit def RequestTo(request : Request[Stream]) = new RequestW(request)
  implicit def RequestFrom(w : RequestW) = w.request
}

trait RestfulResource {
  import com.scapps.experimental.Routing._
  import com.scapps.experimental.OptionKleisli.OptionKleisli
  import RequestW._

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
        dir("/") >=> withParam("edit") >=> (_.asItemRequest) >=> formForEdit _,
        dir("/") >=> (_.asItemRequest) >=> item _
        ),
      PUT >=> dir("/") >=> (_.asItemRequest) >=> updateItem _,
      POST >=> "/" >=> createItem _,
      DELETE >=> dir("/") >=> (_.asItemRequest) >=> deleteItem _
      )
  }
}
