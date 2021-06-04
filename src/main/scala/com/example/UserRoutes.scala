package com.example

import akka.http.scaladsl.server.Directives.{path, _}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
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

  def getUsers(): Future[Users] =
    userRegistry.ask(GetUsers)
  def getUser(name: String): Future[GetUserResponse] =
    userRegistry.ask(GetUser(name, _))
  def createUser(user: User): Future[ActionPerformed] =
    userRegistry.ask(CreateUser(user, _))
  def deleteUser(name: String): Future[ActionPerformed] =
    userRegistry.ask(DeleteUser(name, _))
  def updateUser(user: User): Future[ActionPerformed] =
    userRegistry.ask(UpdateUser(user, _))
  def logIn(username: String,password: String): Future[ActionPerformed] =
  userRegistry.ask(LogIn(username,password, _))

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
                  complete((StatusCodes.Created, performed))
                }
              }
            },
            put {
              entity(as[User]) { user =>
                onSuccess(updateUser(user)) { performed =>
                  complete((StatusCodes.Created, performed))
                }
              }
            }
              )
            })
        }
        //#users-get-delete
        //#users-get-post
        path(Segment) { name =>
          concat(
            get {
              //#retrieve-user-info
              onSuccess(getUser(name)){ performed =>
                complete((StatusCodes.Created, performed))

              }
              //#retrieve-user-info
            },
            delete {
              //#users-delete-logic
              onSuccess(deleteUser(name)) { performed =>
                complete((StatusCodes.Created,performed))
              }
              //#users-delete-logic
            },
              path(Segment){ password =>
                concat(
              patch {
                onSuccess(logIn(name,password)){performed =>
                  complete((StatusCodes.Created,performed))
                }
              })
            })
        }
      //#users-get-delete
  //#all-routes
}
