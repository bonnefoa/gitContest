package contest
import scala.collection.immutable.IntMap

abstract case class Node(id:Int)

case class RepoNode(idRepo:Int) extends Node(idRepo)
case class UserNode(idUser:Int)  extends Node(idUser)

case class Link (var score:Int,dest:Node) extends Ordered[Link]{
  def compare( that:Link)= (this.score - that.score)
}


import java.util.Date

case class Entite(id:Int)
case class Data(dataId:Int,repoId:Int)extends Entite(dataId)
case class Repo(repoId:Int, name:String, date:Date,parentId:Int) extends Entite(repoId){
  def this(repoId:Int, name:String, date:Date)= this(repoId,name,date,0)
  }
case class Lang(repoId:Int,languages: Map[String,Int])  extends Entite(repoId)


