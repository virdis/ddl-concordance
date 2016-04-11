package com.virdis

import opennlp.tools.sentdetect.SentenceDetectorME
import opennlp.tools.tokenize.WhitespaceTokenizer

/**
  * Created by User: sandeep - Project: ddl-conc.
  */
trait ProcessTokens {


  /**
    * Fixed Map of commonly used abbreviations
    */
  val ABBREVIATIONS = Map(
    "ab"   -> "a.b.",
    "abbr" -> "abbr.",
    "acad" -> "acad.",
    "ad"   -> "a.d.",
    "alt" -> "alt.",
    "assn" -> "assn.",
    "ave" -> "ave.",
    "ba" -> "B.A.".toLowerCase(),
    "bc" -> "B.C.".toLowerCase(),
    "BS".toLowerCase() -> "B.S.".toLowerCase(),
    "Capt".toLowerCase() -> "Capt.".toLowerCase(),
    "Col".toLowerCase() -> "Col.".toLowerCase(),
    "Comdr".toLowerCase() -> "Comdr.".toLowerCase(),
    "Corp.".toLowerCase() -> "Corp.".toLowerCase(),
    "dept" -> "dept.",
    "dist" -> "dist.",
    "dr" -> "dr.",
    "est" -> "est.",
    "eg" -> "e.g.",
    "etc" -> "etc.",
    "Gov".toLowerCase() -> "Gov.".toLowerCase(),
    "ie" -> "i.e.",
    "Jr".toLowerCase() -> "Jr.".toLowerCase(),
    "lat" -> "lat.",
    "Lt".toLowerCase() -> "Lt.".toLowerCase(),
    "MD".toLowerCase() -> "M.D.".toLowerCase(),
    "Mr".toLowerCase() -> "Mr.".toLowerCase(),
    "Mrs".toLowerCase() -> "Mrs.".toLowerCase(),
    "Sgt".toLowerCase() -> "Sgt.".toLowerCase(),
    "sr" -> "sr.",
    "rd" -> "rd.",
    "vs" -> "vs.",
    "rev" -> "rev.",
    "pseud" -> "pseud.",
    "pl" -> "pl.",
    "pa" -> "p.a",
    "pm" -> "p.m",
    "ps" -> "p.s",
    "rip" -> "r.i.p",
    "gen" -> "gen.",
    "hon" -> "hon.",
    "prof" -> "prof.",
    "st" -> "st."

  )

  /**
    * Punctuation Regex
    */
  val PUNCTUATION = "\\p{Punct}"

  /**
    * clean token with the Punctuation regex and return the new token
    * @param str
    * @return
    */
  def removePunctuations(str: String): String = {
    val cleanText = str.replaceAll(PUNCTUATION, "")

    if (ABBREVIATIONS.contains(cleanText)) {
      ABBREVIATIONS(cleanText)
    } else {
      cleanText
    }

  }

  /**
    * Takes buffer of all String data,
    * SentenceDetector, whiteSpaceTokenizer
    * counts the tokens and the lines they occur on
    * @param buffer
    * @param sentenceDetect
    * @param whiteSpaceTokenizer
    * @return Map[String, ResultTokens]
    */

  def processTokens(buffer: StringBuilder, sentenceDetect: SentenceDetectorME, whiteSpaceTokenizer: WhitespaceTokenizer) = {
    val sentences = sentenceDetect.sentDetect(buffer.toString())

    val (_, listSentenceData) = sentences.foldLeft((0, List.empty[SentenceData])) {
      case ((count, acc), s) =>
        (count + 1, SentenceData(count, s) +: acc)
    }


    listSentenceData.foldLeft(Map.empty[String, ResultTokens]) {

      (acc, sentenceData) =>
        val tokens = whiteSpaceTokenizer.tokenize(sentenceData.sentence)

        val innerRes = tokens.foldLeft(Map.empty[String, ResultTokens]) {
          (iacc, tok) =>
            val cleanTok = removePunctuations(tok)

            if (iacc.contains(cleanTok)) {
              val ctok = iacc(cleanTok)
              iacc.updated(cleanTok, ResultTokens(ctok.count + 1, List(sentenceData.lineNo) ++ ctok.occurrences))
            } else if (cleanTok.nonEmpty) {
              iacc.updated(cleanTok, ResultTokens(1, List(sentenceData.lineNo)))
            } else {
              iacc
            }
        }
        mergeMap(acc, innerRes)

      }

    }

  /**
    * method to merge maps m1, m2
    * @param m1 Map[String, ResultTokens]
    * @param m2 Map[String, ResultTokens]
    * @return Map[String, ResultTokens]
    */


  def mergeMap(m1: Map[String, ResultTokens], m2: Map[String, ResultTokens]): Map[String, ResultTokens] = {
    (m1.keySet ++ m2.keySet).foldLeft(Map.empty[String, ResultTokens]) {
      (acc, key) =>
        acc.updated(key, mergeResultTokens(m1.get(key), m2.get(key)))
    }
  }

  /**
    * merge ResultTokens
    * @param r1 ResultTokens
    * @param r2 ResultTokens
    * @return ResultTokens
    */

  def mergeResultTokens(r1: Option[ResultTokens], r2: Option[ResultTokens]): ResultTokens = {
    (r1, r2) match {
      case (Some(rt1), Some(rt2)) => ResultTokens(rt1.count + rt2.count, rt1.occurrences ++ rt2.occurrences)
      case (_, Some(rt2)) => rt2
      case (Some(rt1), _) => rt1
      case _ => throw new Exception("This is an impossible state") // keys are part of either maps, so there should be some values
    }
  }

  /**
    * method to pretty print the result
    * @param res
    */

  def prettyPrinting(res: Map[String, ResultTokens]) = {
    val keys = res.keySet.toList.sorted

    def printOccur(xs: List[Int]): String = {
      if (xs.length == 1) xs.head.toString
      else {
        xs.sorted.foldLeft(new StringBuilder) {
          (acc, ele) =>
            if (acc.isEmpty) {
              acc.append(ele)

            } else {
              acc.append(",")
              acc.append(ele)
            }
        }.toString()
      }
    }

    keys.foreach {
      key =>
        println(s"${key}"+s"  {${res(key).count}:${printOccur(res(key).occurrences)}}")
    }
  }
}

case class SentenceData(lineNo: Int, sentence: String)
case class ResultTokens(count: Int, occurrences: List[Int] = Nil)