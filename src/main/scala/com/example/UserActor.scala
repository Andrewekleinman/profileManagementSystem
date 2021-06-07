package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.io.Tcp.Message
import com.example.UserRegistry.Command
import com.example.UserRegistry.Response.ActionPerformed
import com.example.UserRegistry._

object UserActor {
  object element{
    def apply(): Behavior[Command] = {
      Behaviors.setup(context => new element(context))
    }
  }
  class element(context: ActorContext[Command]) extends AbstractBehavior[Command](context) {
    override def onMessage(message: Command): Behavior[Command] = {
      message match {
        case Command.CreateUser(user, replyTo) =>
            //db operation to set user variables

        case Command.DeleteUser(username:String, replyTo) =>
         //db operation to delete user with given username
        case Command.LogIn(logging: Log, replyTo) =>
          //db operation to check for matching password and username
        case Command.UpdateUser(user,replyTo) =>
          //db operation to set user variables
      }
      Behaviors.same

    }
  }

}
