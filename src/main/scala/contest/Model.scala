/*
DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
Version 2, December 2004

Copyright (C) 2004 Sam Hocevar
14 rue de Plaisance, 75014 Paris, France
Everyone is permitted to copy and distribute verbatim or modified
copies of this license document, and changing it is allowed as long
as the name is changed.

DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. You just DO WHAT THE FUCK YOU WANT TO.
*/

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
case class Lang(repoId:Int,languages: Map[String,Int])  extends Entite(repoId){

  def getTotalLines():Int={
    languages.values.foldLeft(0)(_+_)
  }

  def getMapWithPercent():Map[String,Int]={
    val tot = getTotalLines
    //        languages.flatMap(a=>Map(a._1->a._2*100/tot))
    languages.foldLeft(Map[String,Int]())((a,b)=>a++ Map(b._1 -> b._2*100/tot))
  }

  def getAffinity(that:Lang):Int={
    val map1 = getMapWithPercent
    val map2 = that.getMapWithPercent
    map1.keys.foldLeft(0)((res,key)=> Math.abs(map1.getOrElse(key,0)-map2.getOrElse(key,0))+res)
  }

}

