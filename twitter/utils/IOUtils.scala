package twitter.utils

import twitter.models.Tweet
import twitter.utils.TwitterUtilities

import scala.collection.immutable.HashSet
import scala.io.Source

object IOUtils {

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
   *                                                                                 *
   *                              Read Text Files                                    *
   *                                                                                 *
  \* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  /**
   * Takes a Path and a boolean value, indicating if the path is a resource path.
   * If this is the case, returns a new path containing the resource path root, followed by the given path.
   * If this is not the case (isAResource == false) the original path is just returned.
   *
   * Hint: Use getClass.getClassLoader.getResource to obtain a resource url, whose path can be extracted
   *
   * https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getResource(java.lang.String)
   * https://docs.oracle.com/javase/7/docs/api/java/net/URL.html#getPath()
   *
   * @example getPath("Test.txt",false) => "Test.txt"
   * @example getPath("Test.txt",true) => file:target/scala-2.12/test-classes/"Test.txt"
   */
  private def getPath(path: String, isAResource: Boolean): String = {
    if (isAResource) {
      val url = getClass.getClassLoader.getResource(path)
      if (url != null) url.getPath else null
    }
    else
      path
  }

  /**
   * Reads a text file with TwitterData and transform it to an object structure.
   * Uses [[IOUtils.getPath()]] to obtain the actual path of the text file.
   *
   */
  def twitterDataFromFile[T](path: String, isAResource: Boolean = true): List[Tweet] = {

    val filename = getPath(path, isAResource)
    val tweets= {for (line <- Source.fromFile(filename).getLines) yield TwitterUtilities.parse(line) match{
        case Some(t:Tweet) => t
        case None=>
      }}.toList.asInstanceOf[List[Tweet]]
    tweets
  }

  /**
   * Reads a json file into a Spark RDD.
   */
  def TweetListFromJsonFile[T](path: String, isAResource: Boolean = true)(implicit m: Manifest[T]): List[Tweet] = {
    twitterDataFromFile(path, isAResource)
  }

  def readStopwordsFromFile(path: String, isAResource:Boolean = true):HashSet[String]={

    val filename = getPath(path, isAResource)
    val sWords= {for (line <- Source.fromFile(filename).getLines) yield (line(0)==';') match{
      case false => line
      case _ =>
    }}.toList.asInstanceOf[List[String]]
    HashSet(sWords: _*)
  }
}

