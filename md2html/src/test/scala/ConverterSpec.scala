import java.nio.file.{Files, Paths}

import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.funsuite.AnyFunSuite

class ConverterSpec extends AnyFunSuite {
  test("converts single word") {
    val source = "Word."
    val expected = "<body>\n<p>\nWord.\n</p>\n</body>\n"
    assert(Converter.convertDocument(source).contains(expected))
  }

  test("converts two paragraphs") {
    val source = "a\nb\n\nc"
    val expected = "<body>\n<p>\na\nb\n</p>\n<p>\nc\n</p>\n</body>\n"
    assert(Converter.convertDocument(source).contains(expected))
  }

  test("converts single header") {
    val source = "# Header"
    val expected = "<body>\n<h1>Header</h1>\n</body>\n"
    assert(Converter.convertDocument(source).contains(expected))
  }

  test("converts italics") {
    assert(Converter.convertInline("*text*") == "<em>text</em>")
    assert(Converter.convertInline("_text_") == "<em>text</em>")
    assert(Converter.convertInline("_text*") == "_text*")
  }

  test("converts bold") {
    assert(Converter.convertInline("**text**") == "<strong>text</strong>")
    assert(Converter.convertInline("__text__") == "<strong>text</strong>")
  }

  test("converts bold italic") {
    assert(Converter.convertInline("***text***") == "<strong><em>text</em></strong>")
    assert(Converter.convertInline("___text___") == "<strong><em>text</em></strong>")
  }

  test("converts links") {
    assert(Converter.convertInline("[text](link)") == "<a href=\"link\">text</a>")
  }

  test("converts images") {
    assert(Converter.convertInline("![alt text](image.png)") == "<img src=\"image.png\" alt=\"alt text\"/>")
  }

  test("getBlockquotePrefix") {
    assert(Converter.getBlockquotePrefix(">A") == ">")
    assert(Converter.getBlockquotePrefix("> A") == "> ")
    assert(Converter.getBlockquotePrefix(">>A") == ">>")
    assert(Converter.getBlockquotePrefix(">> Abc def") == ">> ")
    assert(Converter.getBlockquotePrefix("> >  > Abc") == "> >  > ")
    assert(Converter.getBlockquotePrefix(">>>> Abc > jkljlj") == ">>>> ")
  }

  test("converts strikethrough text") {
    assert(Converter.convertInline("~~text~~") == "<del>text</del>")
  }

  test("converts inline code") {
    assert(Converter.convertInline("Inline `code`.") == "Inline <code>code</code>.")
  }

  def readResource(path: String): String =
    Source.fromResource(path).mkString

  test("golden-based tests") {
    val resourcePath = Paths.get(getClass.getResource("/").toURI) // points to src/test/resources
    val goldenInputNames: List[String] = Files.list(resourcePath)
      .iterator()
      .asScala
      .map(_.getFileName.toString)
      .filter(p => p.endsWith(".md"))
      .toList

    goldenInputNames.foreach { inputFileName =>
      val input = readResource(inputFileName)
      val expected = readResource(inputFileName.replace(".md", ".html"))

      val actual = Converter.convertDocument(input)
      assert(actual == expected, s"$inputFileName did not match expected output")
    }
  }
}
