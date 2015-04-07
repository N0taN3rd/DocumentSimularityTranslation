import java.util.Scanner
import scala.collection.mutable.HashMap
import scala.collection.immutable.{HashSet,Iterable}

/**
 * Created by John Berlin on 4/5/2015.
 */
class Document(docName:String) extends Iterable[(String,Int)]{
  private[this] val wordCounts:HashMap[String,Int] = new HashMap()
  private[this] val stopList:HashSet[String] = new HashSet()++List("i",
    "a",
    "an",
    "and",
    "are",
    "as",
    "at",
    "be",
    "by",
    "for",
    "from",
    "he",
    "her",
    "his",
    "if",
    "in",
    "is",
    "it",
    "of",
    "on",
    "or",
    "she",
    "that",
    "the",
    "this",
    "to",
    "was",
    "what",
    "when",
    "where",
    "who",
    "will",
    "with")

  def this(docName: String, documentText: String){
    this(docName)
    val scan = new Scanner(documentText)

    while(scan.hasNext) {
      val s = trim(scan.next)
      if (!(stopList contains s))
        wordCounts += s -> (wordCounts.getOrElse(s, 0) + 1)
    }
  }

  def getDocumentName:String = docName

  def getWordCount(word:String):Int = wordCounts getOrElse(word, 0)

  def iteratorAbove(thresh:Int): Iterator[(String,Int)] = wordCounts filter {case(k,v) => v >= thresh} iterator

  def getAbove(thresh:Int): List[String] =
    wordCounts filter { case(k,v) => v >= thresh } map {case (k,v) =>  k} toList

  private def trim(word: String): String = word.replaceAll("^[^A-Za-z]*", "").replaceAll("[^A-Za-z]*$", "").toLowerCase

  override def iterator: Iterator[(String, Int)] = wordCounts iterator

  override def toString(): String = docName + " "+wordCounts.size
}
