package com.example

//import com.example.UserRegistry.ActionPerformed

//#json-formats
import spray.json.DefaultJsonProtocol

object JsonFormats  {
  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._

  implicit val userJsonFormat = jsonFormat5(User)
  implicit val usersJsonFormat = jsonFormat1(Users)
  implicit val logJsonFormat = jsonFormat2(Log)


  //implicit val actionPerformedJsonFormat = jsonFormat1(ActionPerformed)
}
//#json-formats
