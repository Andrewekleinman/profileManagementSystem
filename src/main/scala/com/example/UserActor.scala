package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.io.Tcp.Message
import com.example.UserRegistry.Command
import com.example.UserRegistry.Response.ActionPerformed
import com.example.UserRegistry._

object UserActor {
  object element {
    def apply(): Behavior[Command] = {
      Behaviors.receive[Command] { (context, message) =>
        message match {
          case Command.CreateUser(user, replyTo) =>
            //db operation to set user variables
            replyTo ! ActionPerformed(s"User ${user.username} created.")
            Behaviors.same

          case Command.DeleteUser(username: String, replyTo) =>
            //db operation to delete user with given username
            replyTo ! ActionPerformed(s"User ${username} deleted.")
            Behaviors.same

          case Command.LogIn(logging: Log, replyTo) =>
            var temp = false //db operation to check for matching password and username
            if (temp)
              replyTo ! ActionPerformed(s"${logging.username} signed in")
            else replyTo ! ActionPerformed("incorrect password")
            Behaviors.same
            
          case Command.UpdateUser(user, replyTo) =>
            //db operation to set user variables
            replyTo ! ActionPerformed(s"User ${user.username} updated")
            Behaviors.same
        }
      }
    }
  }
}
