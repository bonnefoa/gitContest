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


class ParserTest extends Spec with ShouldMatchers {
 describe("Parsers "){
        it("should parse the file"){
              val res = LangParser.readFile("langTest.txt") 
        res should contain(Lang(RepoNode(1),Map("C++"->100,"Scala"->200)))
        res should contain(Lang(RepoNode(2),Map("Objective-C"->500)))
        res should contain(Lang(RepoNode(3),Map("C Sharp"->314)))
        res should contain(Lang(RepoNode(4),Map("Max/MSP"->314)))
        }
 }
}
