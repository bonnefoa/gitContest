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

    val tenBestRepo = getFirstBestRepos 

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
      def getFirstBestRepos():List[Int]={
        def innerLoop(iter:Iterator[List[Link]],res:TreeSet[Int]):TreeSet[Int]={
          if(iter.hasNext){
            val list = iter.next
            if(res.lastKey > list(0).score) innerLoop(iter,res)
              else{
              list.takeWhile(_.score > res.lastKey).map(_.score).foldLeft(res)(_+_)
            }
          }else res
        }
        innerLoop(links.values,TreeSet[Int]()).toList
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
