import java.io.{BufferedReader, FileInputStream, InputStreamReader}

import com.virdis.{ProcessTokens, resourceManager}
import opennlp.tools.sentdetect.{SentenceDetectorME, SentenceModel}
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel, WhitespaceTokenizer}

/**
  * Created by User: sandeep - Project: ddl-conc.
  */
object DDApp extends ProcessTokens {


  def main(args: Array[String]): Unit = {

    println("")
    println("")
    println("Application ready .....".toUpperCase())
    println("Type in your text.")
    println("Once you are done, type :q or :quit to begin processing.")
    println("")
    println("")
    resourceManager.using(new BufferedReader(new InputStreamReader(System.in))) {
      stdInReader =>
        var running = true
        val buffer = new StringBuilder()
        val whiteSpaceTokenizer = WhitespaceTokenizer.INSTANCE

        resourceManager.using(new FileInputStream("lib/en-sent.bin")) {
          trainedSM =>
            val sentenceDetect = new SentenceDetectorME(new SentenceModel(trainedSM))

              while (running) {
                val line = stdInReader.readLine()
                if (line.equals(":quit") || line.equals(":q")) {
                  running = false
                  val resMap = processTokens(buffer, sentenceDetect, whiteSpaceTokenizer)

                  prettyPrinting(resMap)

                } else {
                  buffer.append(line.toLowerCase())
                  buffer.append("\n")
                }
              }
        }

    }

    System.exit(0)
  }
}
