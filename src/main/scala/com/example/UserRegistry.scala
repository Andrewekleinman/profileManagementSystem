package com.example

//#user-registry-actor
import akka.actor.ActorContext
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

import scala.collection.{immutable, mutable}

//#user-case-classes
final case class User(username: String, password: String, firstname: String, lastname: String, email: String)
final case class Users(string: Seq[String])
//#user-case-classes

object UserRegistry {
  // actor protocol
  sealed trait Command
  final case class GetUsers(replyTo: ActorRef[Users]) extends Command
  final case class CreateUser(user: User, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class GetUser(username: String, replyTo: ActorRef[GetUserResponse]) extends Command
  final case class DeleteUser(username: String, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class LogIn(username: String, password:String, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class UpdateUser(user: User, replyTo: ActorRef[ActionPerformed]) extends Command


  final case class GetUserResponse(maybeUser: Option[User])
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = storage(mutable.HashMap.empty)


  private def storage(users: mutable.HashMap[String, User]): Behavior[Command] =
    Behaviors.receiveMessage {
      case GetUsers(replyTo) =>
        replyTo ! Users(users.keys.toSeq)
        Behaviors.same
      case CreateUser(user, replyTo) =>
        if (!users.contains(user.username)) {
          users.addOne(user.username, user)
        }
        replyTo ! ActionPerformed(s"User ${user.username} created.")
        storage(users)
      case DeleteUser(username:String, replyTo) =>
        replyTo ! ActionPerformed(s"User $username deleted.")
        if(users.contains(username)){
          //do the delete
          users.remove(username)
        }
        storage(users)
      case LogIn(username, password, replyTo) =>
        if (users.contains(username)) {
          //users(username) ! msg;
        }
        else println("incorrect username")
        replyTo ! ActionPerformed(s"User $username logged in.")
        Behaviors.same
      case UpdateUser(user,replyTo) =>
        if(users.contains(user.username))
          users(user.username) = user
        replyTo ! ActionPerformed(s"User ${user.username} updated.")
        storage(users)

    }
}
//#user-registry-actor
