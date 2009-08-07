package contest
import java.text.{DateFormat,SimpleDateFormat}


object converter{
    implicit def nodeToLink(node:Node):Link= Link(1,node)
  val formatter = new SimpleDateFormat("yy-MM-dd");
  implicit def stringToDate(str:String)= formatter.parse(str)
}

