import scala.io.Source
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

object Md2Html {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: Md2Html <filename.md|directory>")
      sys.exit(1)
    }

    val inputFileName = args(0)
    if (inputFileName.endsWith(".md")) {
      convertFile(inputFileName)
      return
    }

    val dir = new File(inputFileName)
    if (!dir.exists || !dir.isDirectory) {
      println(s"$inputFileName is neither .md file nor a directory")
      sys.exit(1)
    }

    val inputFileNames: List[String] = dir.list()
      .filter(p => p.endsWith(".md"))
      .toList
    for (inputFileName <- inputFileNames) {
      convertFile(new File(dir, inputFileName).getAbsolutePath())
    }
  }

  def convertFile(inputFileName: String): Unit = {
    assert(inputFileName.endsWith(".md"))
    println(s"Converting $inputFileName...")
    val outputFileName = inputFileName.stripSuffix(".md") + ".html"

    try {
      val source = Source.fromFile(inputFileName)
      val content: String = try {
        source.mkString
      } finally {
        source.close()
      }

      val result: String = Converter.convertDocument(content)

      val writer = new PrintWriter(new File(outputFileName))
      try {
        writer.write(result)
      } finally {
        writer.close()
      }

      println(s"Wrote converted HTML to $outputFileName")
    } catch {
      case e: Exception =>
        println(s"Error while converting: ${e.getMessage}")
        e.printStackTrace()
    }
  }
}