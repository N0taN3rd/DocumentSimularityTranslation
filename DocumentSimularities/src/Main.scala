import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.Source


/**
 * Created by John Berlin on 4/5/2015.
 */
object Main {
   def main (args: Array[String]) {
     val docs = new ListBuffer[Document]()
     var i = 0
     loop {
       docs += new Document(args(i),readDoc(new File(args(i))))
       i += 1
     } unless (i == args.length)

     selectBestMatch(docs)
  }

  def loop(body: => Unit): LoopUnlessCondition = new LoopUnlessCondition(body)

  class LoopUnlessCondition(body: => Unit) {
    def unless(cond: => Boolean): Unit ={
      body
      if(!cond) unless(cond)
    }
  }

  def readDoc(doc:File):String = {
    val build = new StringBuffer()
    (Source.fromFile(doc).getLines()).foreach { case line => build.append(line).append("\n") }
    build.toString
  }

  def selectBestMatch(docs:ListBuffer[Document]) = {
    var bestScore = -1.0
    var bestMatchingDoc = "no document found"
    for(i <- 1 until docs.size){
        val score = new Similarity(docs head ,docs apply i) getSimilarity 4
        if(score > bestScore){
          bestScore = score
          bestMatchingDoc = docs apply i getDocumentName
        }
    }

    println("Best match is " + bestMatchingDoc + " with a similarity of ")
    printf("%.2f%n",bestScore)
  }
}
