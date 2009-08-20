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

import org.scalatest._
import matchers._
import org.scalacheck._
import Prop._
import java.io.{File, ByteArrayOutputStream, FileInputStream}
import scala.io.Source
import contest.converter._
import org.mockito.Mockito._
import scala.collection.immutable._

import scala.collection.mutable.MultiMap
import scala.collection.mutable.Set
import scala.collection.immutable.HashMap  
import scala.collection.immutable.IntMap


class LangTest extends Spec with ShouldMatchers with TestEnvironnement3{

  describe("The lang algorithme "){
    it("should find 1000 lines from Lang"){
      Lang(RepoNode(1),Map("Java"->400,"Groovy"->100,"Scala"->500)).getTotalLines should be(1000)
    }
    it("should find correct percent"){
      Lang(RepoNode(1),Map("Java"->400,"Groovy"->100,"Scala"->500)).getMapWithPercent should be(
        Map("Java"->40,"Groovy"->10,"Scala"->50)
      )
  }

  it("should get the percent of all repo of a user"){
//        graph.     
  }

  it("should sort list link with lang affinity"){
  }
}
  }
  class GraphTest extends Spec with ShouldMatchers with TestEnvironnement{
    describe("A treeSet of links"){
      it("should have the higher score to the right"){
        TreeSet(Link(1,repo1),Link(14,repo2),Link(5,repo3)).toList should be (List(Link(14,repo2),Link(5,repo3),Link(1,repo1)).reverse)
      }
    }
    describe("The Graph initializer"){
      it("should parse the graph the graph"){ 
        val map = Initialise.parseDataToGraph(Initialise.readFile("dataTest.txt"),HashMap())
          map should contain value (List(Link(1,repo2),Link(1,repo1)))
        map should contain value (List(Link(1,repo2),Link(1,repo1)))
        map should contain value (List(Link(1,user1)))
        map should contain value (List(Link(1,user2),Link(1,user1)))
        map should contain value (List(Link(1,repo2)))
      }
      it("should score the user1 list of link correctly"){
        val listLinkScored = Initialise.scoreLinks(user1,mapRef)
          listLinkScored should contain (Link(3,repo2))
        listLinkScored should contain (Link(2,repo1))
      }

      it("should score the user2 list of link correctly"){
        val listLinkScored = Initialise.scoreLinks(user2,mapRef)
          listLinkScored should contain (Link(3,repo2))
      }

      it("should score the repo1 list of link correctly"){
        val listLinkScored = Initialise.scoreLinks(repo1,mapRef)
          listLinkScored should contain (Link(3,user1))
      }

      it("should score the map"){
        val mapScored=Initialise.scoreGraph(mapRef)
          mapScored should contain value (List(Link(3,repo2),Link(2,repo1)))
        mapScored should contain value (List(Link(3,user1)))
        mapScored should contain value (List(Link(3,user1),Link(2,user2)))
        mapScored should contain value (List(Link(3,repo2)))
      }
      it("should sort the map"){
        val mapSortedAndScored = Initialise.sortLinks(littleMap)
          mapSortedAndScored should contain value (List(Link(3,repo2),Link(40,repo1),Link(1,repo1)).sort(_>_))
      }
      it("should remove useless links"){
        val mapPurged = Initialise.removeUselessLinks(HashMap(user1-> List.range(1, 20).map(Link(_,repo1))))
          mapPurged should contain value( List.range(1, 11).map(Link(_,repo1)) )
      }
      it("should get the 10 best repos"){
        val map = HashMap[Node,List[Link]](user1 -> List.range(1,50).map(a=>Link(a*2,RepoNode(a*2))),user2->List.range(1,50).map(a=>Link(a*2+1,RepoNode(a*2+1))))
          val grap = new Graph(Initialise.sortLinks(map))
          val res = grap.getFirstBestRepos().map(_.dest.id).toList
        res should be((90 to 99).toList.reverse)
      }
      it("should not duplicate the best repos"){
        val map = HashMap[Node,List[Link]](user1 -> List.range(1,50).map(a=>Link(a*2,RepoNode(a*2))),user2->List.range(1,50).map(a=>Link(a*2,RepoNode(a*2))))
          val grap = new Graph(Initialise.sortLinks(map))
          val res = grap.getFirstBestRepos().map(_.dest.id).toList
        res should be ( (80 to 98 by 2).toList.reverse )

      }
      it("should get the repo id and not the score"){
        val map = HashMap[Node,List[Link]](user1 -> List.range(1,50).map(a=>Link(a*2,RepoNode(a))))
          val grap = new Graph(Initialise.sortLinks(map))
          val res = grap.getFirstBestRepos().map(_.dest.id).toList
        res should be ( (40 to 49).toList.reverse)
      }
    }
    describe("Search the best candidates algorithm"){
      it("should find nothing if there is only first degree repos"){
        val mapRef = HashMap[Node,List[Link]](
          user1->List(Link(1,repo1),Link(1,repo2)),
          repo1->List(Link(2,user1)),
          repo2->List(Link(2,user1))
        )
      val graph= new Graph(mapRef)
        graph.getBestCandidates(1) should be (Nil)
    }
    it("should find the repo on second degree"){
      val mapRef = HashMap[Node,List[Link]](
        user1->List(Link(2,repo1)),
        user2->List(Link(2,repo1),Link(1,repo2)),
        repo1->List(Link(1,user1),Link(2,user2)),
        repo2->List(Link(2,user2))
      )
    val graph= new Graph(mapRef)
      graph.getBestCandidates(1) should be (List(2))
  }

  it("should find the repo on the third link"){
    val graph=Initialise.initialiseGraph("thirdLink.txt")
      graph.getBestCandidates(1) should be (List(2,3))
  }
  it("should find repos in the right order from dataset3"){
    val graph=Initialise.initialiseGraph("dataTest3.txt")
      graph.getBestCandidates(1).sort(_<_) should be ((2 to 11).toList)
  }
}
}
trait TestEnvironnement3 extends GraphComponent{
  val graph=new Graph(
        Initialise.parseDataToGraph(Initialise.readFile("dataTest4.txt"),HashMap()),
        Initialise.parseLang("langDataTest4.txt")
      )
}

trait TestEnvironnement extends GraphComponent{
  val graph=new Graph(Initialise.parseDataToGraph(Initialise.readFile("dataTest.txt"),HashMap()))
    val user1 = new UserNode(1)
    val user2 = new UserNode(2)
    val user3 = new UserNode(3)
    val repo1 = new RepoNode(1)
    val repo2 = new RepoNode(2)
    val repo3 = new RepoNode(3)
    val repo4 = new RepoNode(4)
    val repo5 = new RepoNode(5)
    val repo6 = new RepoNode(6)
    val repo7 = new RepoNode(7)

    val mapRef= HashMap[Node,List[Link]](
    user1 -> List(Link(1,repo2),Link(1,repo1)),
    repo1 -> List(Link(1,user1)),
    repo2 -> List(Link(1,user1),Link(1,user2)),
    user2 -> List(Link(1,repo2))
  )

val littleMap=HashMap[Node,List[Link]](
  user1-> List(Link(3,repo2),Link(40,repo1),Link(1,repo1))
)

val map= HashMap[Node,List[Link]]( 
  user1 -> List(Link(3,repo2),Link(2,repo1)),
  repo1 -> List(Link(3,user1)),
  repo2 -> List(Link(3,user1),Link(2,user2)),
  user2 -> List(Link(3,repo2))
)
}


