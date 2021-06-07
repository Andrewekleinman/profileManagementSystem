package com.example

//#user-registry-actor
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl._
import com.example.UserRegistry.Response.{ActionPerformed, GetUserResponse}
//import com.example.UserRegistry.element
import akka.actor.typed.{ActorRef, ActorSystem}

import scala.collection.mutable

//#user-case-classes
final case class User(username: String, password: String, firstname: String, lastname: String, email: String)
final case class Users(string: Seq[String])

final case class Log(username: String, password: String)

//#user-case-classes

object UserRegistry {
  // actor protocol
  sealed trait Command
  object Command {
    final case class GetUsers(replyTo: ActorRef[Users]) extends Command
    final case class CreateUser(user: User, replyTo: ActorRef[ActionPerformed]) extends Command
    final case class GetUser(username: String, replyTo: ActorRef[GetUserResponse]) extends Command
    final case class DeleteUser(username: String, replyTo: ActorRef[ActionPerformed]) extends Command
    final case class LogIn(logging: Log, replyTo: ActorRef[ActionPerformed]) extends Command
    final case class UpdateUser(user: User, replyTo: ActorRef[ActionPerformed]) extends Command
  }

  sealed trait Response
  object Response {
    final case class GetUserResponse(maybeUser: Option[User])
    final case class ActionPerformed(description: String)
  }
  def apply(): Behavior[Command] = mainBehavior(mutable.HashMap.empty)


  private def mainBehavior(users: mutable.HashMap[String, ActorRef[User]]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case Command.GetUsers(replyTo) =>
          replyTo ! Users(users.keys.toSeq)
          Behaviors.same
        case Command.CreateUser(user, replyTo) =>
          if (!users.contains(user.username)) {

            val newUser = context.spawn(element(),user.username)
            users.addOne(user.username, newUser)
            newUser ! user
          }
          replyTo ! ActionPerformed(s"User ${user.username} created.")
          mainBehavior(users)
        case Command.DeleteUser(username:String, replyTo) =>
          replyTo ! ActionPerformed(s"User $username deleted.")
          if(users.contains(username)){
            users(username) ! username
            context.stop(users(username))
            users.remove(username)
          }
          mainBehavior(users)
        case Command.LogIn(logging: Log, replyTo) =>
          if (users.contains(logging.username)) {
            users(logging.username) ! logging
            replyTo ! ActionPerformed("login confirmed")
          }
          else replyTo ! ActionPerformed("incorrect username")
          Behaviors.same
        case Command.UpdateUser(user,replyTo) =>
          if(users.contains(user.username)) {
            context.stop(users(user.username))
            users(user.username) = context.spawn(element(),user.username)
            users(user.username) ! user
            replyTo ! ActionPerformed(s"User ${user.username} updated.")
          }
          replyTo ! ActionPerformed("that user does not exist")
          mainBehavior(users)
      }
    }

}
object element{
  def apply(): Behavior[Any] = {
    Behaviors.receive { (context, msg) =>
      msg match {
        case msg.getClass == User => {
          //db operation to change change the user
        }
        case msg.getClass == Log => {
          if (true /*db operation to confirm password*/ )
            context.log.info("logged in")
          else context.log.info("incorrect username or password")
        }
        case msg.getClass == String =>
        //db operation to remove element who's ID was passed
      }
      Behaviors.same
    }

  }
}