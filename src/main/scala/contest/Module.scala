package contest

import scala.collection.immutable.HashMap  

object ComponentRegistry extends   ResultWriterComponent with GraphComponent
{  
  val resultWriter= new ResultWriter
//  val graph = new Graph(new HashMap[Node,List[Link]])
  val graph = Initialise.initialiseGraph("data.txt")
}

