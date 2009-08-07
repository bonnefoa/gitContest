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

import scala.io.Source
import java.io._
object GitContest extends Application {
  println("Writing result...")
  ComponentRegistry.resultWriter.writeResult("test.txt","result.txt")
}

trait ResultWriterComponent{
  this:GraphComponent=>
  val resultWriter:ResultWriter
  class ResultWriter {
    def writeResult(testFile:String,outputFile:String){
      val out = new BufferedWriter(new FileWriter(outputFile));
      Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(testFile)).getLines.foreach{
        string=>{
          val userId= string.stripLineEnd
          val line=userId+graph.getBestCandidates(userId.toInt).mkString(":",",","")
            println(line)
          out.write(line)
          out.newLine
        }
      }
      out.close
    }
  }
}


