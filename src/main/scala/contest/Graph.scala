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

  case class Graph(links:NodeGraph){

    lazy val tenBestRepo = getFirstBestRepos 

    def getBestCandidatesOrElse10Top(userId:Int):List[Int]={
      val res = getBestCandidates(UserNode(userId))
        if (res.size==0) tenBestRepo.map(_.dest.id).toList
      else if(res.size < 10) (res++tenBestRepo).map(_.dest.id).toList.takeRight(10)
        else res.map(_.dest.id).toList.takeRight(10)
    }

    def getBestCandidates(userId:Int):List[Int]={ getBestCandidates(UserNode(userId)).sort(_>_).map(_.dest.id).toList.takeRight(10) }

    def getBestCandidates(userNode:UserNode):List[Link]={
    if(!links.isDefinedAt(userNode)) Nil
    else{
      val firstDegreeRepos=links(userNode)
        def innerLoop(currentNode:Node,nextLinkToVisit:Set[Link], linkToVisit:Set[Link],nodeVisited:List[Node],res:List[Link]):List[Link]={
        res.removeDuplicates
        res.sort(_<_)
        val newNodeVisited=currentNode::nodeVisited
        val temp = linkToVisit++links(currentNode).filter(link=>(!newNodeVisited.contains(link.dest)))

          val nextFiltered =  nextLinkToVisit.filter(link=>(!newNodeVisited.contains(link.dest)))
          val (newNextLinkToVisit,newLinkToVisit) = 
          if(res.size >10) (nextFiltered,TreeSet[Link]())
              else if(nextFiltered.size == 0) (temp,TreeSet[Link]())
            else (nextFiltered,temp)

        if(newNextLinkToVisit.size == 0) res
        else{
          val nextLink = newNextLinkToVisit.elements.next
          currentNode match {
            case UserNode(a)=>{
              innerLoop(nextLink.dest,newNextLinkToVisit,newLinkToVisit-nextLink,newNodeVisited,res:::links(currentNode)--firstDegreeRepos)
            }
            case RepoNode(b)=>
            innerLoop(nextLink.dest,newNextLinkToVisit,newLinkToVisit-nextLink,newNodeVisited,res)
          }
        }
      }
      innerLoop(userNode, TreeSet(),TreeSet(),Nil,Nil).removeDuplicates
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

def readFile(file:String):Iterator[Data]={
  DataParser.readFile(file).elements
}

def initialiseGraph(file:String):Graph={
  new Graph(
    sortLinks(
      removeUselessLinks(
        scoreGraph(
          parseDataToGraph(readFile(file), HashMap()
          )))))
      }
    }

  }
