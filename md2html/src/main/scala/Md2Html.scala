import scala.io.Source
import java.io.{File, PrintWriter}

object Md2Html {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: Md2Html <filename.md>")
      sys.exit(1)
    }

    val inputFileName = args(0)
    if (!inputFileName.endsWith(".md")) {
      println("Error: input file must have .md extension")
      sys.exit(1)
    }

    val outputFileName = inputFileName.stripSuffix(".md") + ".html"

    try {
      val source = Source.fromFile(inputFileName)
      val content: String = try {
        source.mkString
      } finally {
        source.close()
      }

      val result: String = Converter.convert(content)

      val writer = new PrintWriter(new File(outputFileName))
      try {
        writer.write(result)
      } finally {
        writer.close()
      }

      println(s"Wrote converted HTML to $outputFileName")
    } catch {
      case e: Exception =>
        println(s"Error: ${e.getMessage}")
    }
  }
}