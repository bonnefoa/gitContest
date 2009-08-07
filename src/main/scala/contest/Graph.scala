package contest

import scala.collection.immutable.HashMap  
import scala.collection.immutable.IntMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map

import scala.io.Source
import java.io._
import scala.collection.immutable.SortedSet  
import scala.collection.immutable.TreeSet
import scala.collection.immutable.SortedMap
import scala.collection.immutable.EmptyMap

trait GraphComponent{
  val graph:Graph
  type NodeGraph =Map[Node,List[Link]]

  case class Graph(links:NodeGraph){

    def getBestCandidates(userId:Int):List[Int]={ getBestCandidates(UserNode(userId)) }

    def getBestCandidates(userNode:UserNode):List[Int]={
      if(!links.isDefinedAt(userNode)) Nil
      else{

        val firstDegreeRepos=links(userNode).map(_.dest.id)
          def innerLoop(currentNode:Node,linkToVisit:List[Link],nodeVisited:List[Node],res:List[Int]):List[Int]={
          if(res.size >10)res
          else{
            val newNodeVisited=currentNode::nodeVisited
            val newLinkToVisit= (linkToVisit:::(links(currentNode).take(10))).remove((link)=>newNodeVisited.contains(link.dest) )
              if(newLinkToVisit==Nil)
              res
            else
              currentNode match {
              case UserNode(a)=>{
                val result = res:::links(currentNode).map(_.dest.id)--firstDegreeRepos
                innerLoop(newLinkToVisit(0).dest,newLinkToVisit,newNodeVisited,result)
              }
              case RepoNode(b)=>
              innerLoop(newLinkToVisit(0).dest,newLinkToVisit,newNodeVisited,res)

            }
          }
        }
        innerLoop(userNode, Nil,Nil,Nil).take(10)
      }}
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

  def sortLinks(links:NodeGraph):NodeGraph={
    def innerLoop(iter:Iterator[Node],links:NodeGraph):NodeGraph={
      if(iter.hasNext){
        val node = iter.next
        innerLoop(iter,links.update(node,links(node).sort(_>_)))
      }
      else links
    }
    innerLoop(links.keys,links)
  }

  def scoreGraph(links:NodeGraph):NodeGraph={
    def innerLoop(iter:Iterator[Node],links:NodeGraph):NodeGraph={
      if(iter.hasNext){
        val node=iter.next
        innerLoop(iter, 
          links.update(node,scoreLinks(node,links))
        )
    }
    else links
  }
  innerLoop(links.keys,links)
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
      scoreGraph(
        parseDataToGraph(readFile(file), HashMap()
        ))))
    }
  }
}
