package com.example
//curl -X POST http://127.0.0.1:8080/users -d '{"username": "a","password":"b","firstname": "c","lastname":"d","email":"e"}' -H "Content-Type:application/json"
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.javadsl.model.headers.RawHeader
import akka.http.scaladsl.model.{StatusCodes, headers}
import akka.http.scaladsl.model.headers.{HttpOrigin, Origin, RawHeader}
import akka.http.scaladsl.server.Directives.{path, _}
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.example.UserRegistry._

import scala.concurrent.{ExecutionContextExecutor, Future}

//#import-json-formats
//#user-routes-class
class UserRoutes(userRegistry: ActorRef[UserRegistry.Command])(implicit val system: ActorSystem[_]) {

  //#user-routes-class
  import JsonFormats._
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
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
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")){
              complete(getUsers())}
            },
            options {
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")){
              complete(StatusCodes.OK)}
            },
            post {
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")){
                entity(as[User]) { user =>
                onComplete(createUser(user)) { performed =>
                   complete(StatusCodes.OK)
                  }
                }
              }
            },
            put {
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")){

                entity(as[User]) { user =>
                  onSuccess(updateUser(user)) { performed =>
                  complete(StatusCodes.OK)}
                }
              }
            },
            patch {
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")){

                entity(as[Log]) { log =>
                  onSuccess(logIn(log)) { performed =>
                  complete(StatusCodes.OK)}
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
              respondWithHeaders(headers.RawHeader("Access-Control-Allow-Origin", "*"),headers.RawHeader("Access-Control-Allow-Headers","content-type"),headers.RawHeader("Access-Control-Allow-Methods","*")) {
                //#users-delete-logic
                onSuccess(deleteUser(name)) { performed =>
                  complete((StatusCodes.NoContent))
                }
              }              //#users-delete-logic
            })
        })
    }
      //#users-get-delete
  //#all-routes
}

