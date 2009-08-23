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

import scala.collection.immutable.HashMap  
import scala.collection.immutable.IntMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.collection.immutable.HashSet
import scala.collection.immutable.EmptySet

import scala.io.Source
import scala.collection.immutable.EmptyMap

trait GraphComponent{
  val graph:Graph
  type NodeGraph =Map[Node,List[Link]]

  case class Graph(links:NodeGraph, mapLang:Map[Node,Lang]){
    def this(links:NodeGraph)=this(links,Map[Node,Lang]())
      //    lazy val tenBestRepo = getFirstBestRepos 

    //    implicit lazy val mapLang = Initialise.parseLang(langFile)

    //    def getBestCandidatesOrElse10Top(userId:Int):List[Int]={
      //      val res = getBestCandidates(UserNode(userId))
      //        if (res.size==0) tenBestRepo.map(_.dest.id).toList
      //      else if(res.size < 10) (res++tenBestRepo).map(_.dest.id).toList.takeRight(10)
      //        else res.map(_.dest.id).toList.takeRight(10)
      //    }

      def getBestCandidates(userId:Int):List[Int]={ 
        getBestCandidatesByScore(UserNode(userId)).map(_.dest.id).toList.take(10)
      }

      def getBestCandidatesByScoreAndByLanguages(userId:Int):List[Int]={
        val userNode=UserNode(userId)
        val res = getBestCandidatesByScore(userNode).removeDuplicates.take(20)
        Initialise.scoreListWithLangAffinity(userNode,res)(mapLang).sort(_<_).map(_.dest.id).toList.take(10)
      }

      def getBestCandidatesByScore(userNode:UserNode):List[Link]={
        if(!links.isDefinedAt(userNode)) Nil
        else{
          val firstDegreeRepos=links(userNode)
            def innerLoop(currentNode:Node,linkToVisit:Set[Link],nodeVisited:List[Node],res:List[Link]):List[Link]={
            val newRes = res.removeDuplicates.sort(_>_)
              if(newRes.size >10) newRes
            else{
              val newNodeVisited=currentNode::nodeVisited
              val newLinkToVisit= (linkToVisit++links(currentNode)).filter(link=>(!newNodeVisited.contains(link.dest)))
                if(newLinkToVisit.size == 0)newRes 
              else{
                val nextLink = newLinkToVisit.elements.next
                currentNode match {
                  case UserNode(a)=>{
                    innerLoop(nextLink.dest,newLinkToVisit-nextLink,newNodeVisited,newRes:::links(currentNode)--firstDegreeRepos)
                  }
                  case RepoNode(b)=>
                  innerLoop(nextLink.dest,newLinkToVisit-nextLink,newNodeVisited,newRes)
                }
              }
            }
          }
          innerLoop(userNode, TreeSet(),Nil,Nil).removeDuplicates
        }
      }

      def getFirstBestRepos():Set[Link]={
        def innerLoop(iter:Iterator[List[Link]],res:SortedSet[Link] ):SortedSet[Link]={
          if(iter.hasNext){
            val list = iter.next
            val head = res.firstKey
            if(head > list(0)) innerLoop(iter,res)
              else{
              innerLoop(iter,
                TreeSet[Link]()++(res++list.takeWhile(_ > head)).toList.takeRight(10)
              )
          }
        }else res
      }
      innerLoop(links.values, TreeSet[Link](Link(0,RepoNode(0))))
    }
  }

  object Initialise{

    def parseDataToGraph(iter:Iterator[Data],links:NodeGraph):NodeGraph={
      if (iter.hasNext){
        val data=iter.next
        val uNode=UserNode(data.dataId)
          val rNode=RepoNode(data.repoId)
          parseDataToGraph(  
          iter,
          links.update(rNode,
            new Link(1,uNode)::links.getOrElse(rNode,Nil)).
          update(uNode,
            new Link(1,rNode)::links.getOrElse(uNode,Nil) 
          )
      )
  }else links
}

def parseLang(file:String):Map[Node,Lang]={
  LangParser.readFile(file).foldLeft(Map[Node,Lang]())((map,lang)=> map++Map(lang.node->lang))
}

def sumListMap(listMap: List[Map[String,Int]]):Map[String,Int]={
  println("ga")
  listMap.foldLeft(Map[String,Int]())(
    (map,current)=> 
    current.keys.foldLeft(map)((map,key)=>
      map.get(key) match { 
        case None => map.update(key, current(key)); 
        case Some(oldvalue) => map.update(key, oldvalue+current(key)) 
      }
    )
)
}

def processMapLangToPercent(map:Map[Node,Lang]):Map[Node,Lang]={
  map.foldLeft(Map[Node,Lang]())((res,current)=>
        res.update(current._1,current._2.getLangWithPercent)
        )
}

def processLangToFillUserNode(links:Map[Node,List[Link]],map:Map[Node,Lang]):Map[Node,Lang]={
  links.elements.
  /* filter(key=> key._1 match {
      case a:UserNode =>true
      case _ => false
    }) 
  */
  foldLeft(map)((res,current)=>{
      current._1 match {
        case uNode:UserNode =>{
          val listMap = current._2.foldLeft(List[Map[String,Int]]())((listRes,cur)=>map(cur.dest).languages::listRes)
            res.update(current._1,new Lang(current._1,sumListMap(listMap)))
        }
        case _ =>res  
      }
}
    )
}
def applyToMap(links:NodeGraph,f:(Node, NodeGraph)=> NodeGraph):NodeGraph={
  def innerLoop(iter:Iterator[Node],links:NodeGraph):NodeGraph={
    if(iter.hasNext){
      val node = iter.next
      innerLoop(iter,f(node,links))
    }
    else links
  }
  innerLoop(links.keys,links)
}

def sortLinks(links:NodeGraph):NodeGraph={
  applyToMap(links, (node,links)=> links.update(node,links(node).sort(_>_)))
}

def removeUselessLinks(links:NodeGraph):NodeGraph={
  applyToMap(links, (node,links)=> links.update(node,links(node).take(10)))
}

def scoreGraph(links:NodeGraph):NodeGraph={
  applyToMap(links,(node,links)=> links.update(node,scoreLinks(node,links)))
}

def scoreLinks(node:Node,map:NodeGraph):List[Link]={
  map(node).flatMap(link=> Link(map(link.dest).size+link.score,link.dest) ::Nil)
}

def scoreListWithLangAffinity(userNode:UserNode,linksToSort:List[Link])(implicit theMapLang:Map[Node,Lang]):List[Link]={
  val refLang =  theMapLang(userNode)
    linksToSort.foldLeft(Nil:List[Link]) ( (b,a)=> Link(refLang.getAffinity(theMapLang(a.dest)),a.dest)::b)
}

def readFile(file:String):Iterator[Data]={
  DataParser.readFile(file).elements
}

def initialiseGraph(fileData:String):Graph={
  new Graph(
    sortLinks(
      removeUselessLinks(
        scoreGraph(
          parseDataToGraph(readFile(fileData), HashMap()
          ))))
      )
  }
  def initialiseGraph(fileData:String,fileLang:String):Graph={
    val links=parseDataToGraph(readFile(fileData), HashMap())
      new Graph(
      sortLinks(
        removeUselessLinks(
          scoreGraph(links
          )))
        ,
        processLangToFillUserNode(links,parseLang(fileLang))
      )
  }
}
    }
