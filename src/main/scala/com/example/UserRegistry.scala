package com.example

//#user-registry-actor
import akka.actor.ActorContext
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.UserRegistry.Response.{ActionPerformed, GetUserResponse}
//import com.example.UserRegistry.element

import scala.collection.{immutable, mutable}

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

           // val newUser = context.spawn(element(),user.username)
//            users.addOne(user.username, newUser)
//            newUser ! user
          }
          replyTo ! ActionPerformed(s"User ${user.username} created.")
          mainBehavior(users)
        case Command.DeleteUser(username:String, replyTo) =>
          replyTo ! ActionPerformed(s"User $username deleted.")
          if(users.contains(username)){
            //database operation to delete
            context.stop(users(username))
            users.remove(username)
          }
          mainBehavior(users)
        case Command.LogIn(logging: Log, replyTo) =>
          if (users.contains(logging.username)) {
            //database operation to check for password match
            replyTo ! ActionPerformed("login confirmed")
          }
          else replyTo !ActionPerformed("incorrect username")
          Behaviors.same
        case Command.UpdateUser(user,replyTo) =>
          if(users.contains(user.username)) {
            users(user.username) ! user
            context.stop(users(user.username))
            //users(user.username) = context.spawn(element(),user.username)
            replyTo ! ActionPerformed(s"User ${user.username} updated.")
          }
          replyTo ! ActionPerformed("that user does not exist")
          mainBehavior(users)
      }
    }

}
object element{
  def apply(): Behavior[String] = {
    Behaviors.setup(context => new element(context))
  }
}
class element(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  override def onMessage(msg: String): Behavior[String] = {
    val arr = msg.split(" ");
    arr(0) match {
      case "create" =>
      //db operation add element
      case "update" =>
      //db operation update element
      case "delete" =>
      //db operation remove element if username and password match
      case "login" =>
      //db operation query password for username, compare
      //if it doesn't match, print "incorrect password"
    }
    this
  }
}

