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


