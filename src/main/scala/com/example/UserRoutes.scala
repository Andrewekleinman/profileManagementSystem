package com.example

import akka.http.scaladsl.server.Directives.{path, _}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.{ExecutionContextExecutor, Future}
import com.example.UserRegistry._
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.util.Timeout

//#import-json-formats
//#user-routes-class
class UserRoutes(userRegistry: ActorRef[UserRegistry.Command])(implicit val system: ActorSystem[_]) {

  //#user-routes-class
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._
  //#import-json-formats

  // If ask takes more time than this to complete the request is failed
  private implicit val timeout = Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))
  private implicit val ec: ExecutionContextExecutor = system.executionContext


  def getUsers(): Future[Users] = userRegistry.ask(Command.GetUsers)
  def getUser(name: String): Future[Response.GetUserResponse] = userRegistry.ask(Command.GetUser(name, _))
  def createUser(user: User): Future[Response.ActionPerformed] = userRegistry.ask(Command.CreateUser(user, _))
  def deleteUser(name: String): Future[Response.ActionPerformed] = userRegistry.ask(Command.DeleteUser(name, _))
  def updateUser(user: User): Future[Response.ActionPerformed] = userRegistry.ask(Command.UpdateUser(user, _))
  def logIn(logging: Log): Future[Response.ActionPerformed] = userRegistry.ask(Command.LogIn(logging, _))

  //#all-routes
  //#users-get-post
  //#users-get-delete
  val userRoutes: Route =
    pathPrefix("users") {
      concat(
        //#users-get-delete
        pathEnd {
          concat(
            get {
              complete(getUsers())
            },
            post {
              entity(as[User]) { user =>
                onSuccess(createUser(user)) { performed =>
                  complete(StatusCodes.OK)
                }
              }
            },
            put {
              entity(as[User]) { user =>
                onSuccess(updateUser(user)) { performed =>
                  complete(StatusCodes.OK)
                }
              }
            },
            patch {
              entity(as[Log]) { log =>
                onSuccess(logIn(log)) { performed =>
                  complete(StatusCodes.OK)
                }
              }
            }
              )
            },
        //#users-get-delete
        //#users-get-post
        path(Segment) { name =>
          concat(
            delete {
              //#users-delete-logic
              onSuccess(deleteUser(name)) { performed =>
                complete((StatusCodes.NoContent))
              }
              //#users-delete-logic
            })
        })
    }
      //#users-get-delete
  //#all-routes
}

