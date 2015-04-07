import scala.collection.mutable

/**
 * Created by John Berlin on 4/5/2015.
 */
class Similarity(d1:Document,d2:Document) {
  var indexTerms:mutable.HashSet[String] =  new mutable.HashSet()

  def getIndexTerms:mutable.HashSet[String] = {
    computeIndexTerms(1)
    indexTerms
  }

  def getSimilarity(threshold:Int): Double = {
    computeIndexTerms(threshold)
    val totalCount = indexTerms size

  /*
    first implementation
    indexTerms foreach {
        case x =>
          if(d1.getWordCount(x) >= threshold && d2.getWordCount(x) >=  threshold)
            sim += 1.0
    }

    indexTerms.foldLeft(0.0) {
      (a,b) => {
        if (d1.getWordCount(b) >= threshold && d2.getWordCount(b) >= threshold)
          a + 1.0
        else
          a
      }
    }

     (indexTerms filter (x => d1.getWordCount(x) >= threshold && d2.getWordCount(x) >= threshold)).foldLeft(0.0)((a:Double,b:String)=>{
        a + 1.0
      })
    */
    //this is a the coolest of the commented out ways to get a similarity score
    var sim = (0.0 /: indexTerms)( (a,b) => {
      if (d1.getWordCount(b) >= threshold && d2.getWordCount(b) >= threshold)
        a + 1.0
      else
        a
    })

    println(sim)
    if(totalCount > 0)
      sim /= totalCount
    sim
  }

  private def computeIndexTerms(threshHold:Int) =
    indexTerms ++= d1 getAbove threshHold  ++= d2 getAbove threshHold

}
