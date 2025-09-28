import java.nio.file.{Files, Paths}

import scala.io.Source
import scala.jdk.CollectionConverters._

import org.scalatest.funsuite.AnyFunSuite

class ConverterSpec extends AnyFunSuite {

  def extractBody(html: String): String = {
    val bodyPattern = "(?s)<body[^>]*>(.*?)</body>".r
    bodyPattern.findFirstMatchIn(html).map(_.group(1)).get.trim
  }

  test("converts single word") {
    val source = "Word."
    val expected = "<p>\nWord.\n</p>"
    assert(extractBody(Converter.convertDocument(source)) == expected)
  }

  test("converts two paragraphs") {
    val source = "a\nb\n\nc"
    val expected = "<p>\na\nb\n</p>\n<p>\nc\n</p>"
    assert(extractBody(Converter.convertDocument(source)) == expected)
  }

  test("converts single header") {
    val source = "# Header"
    val expected = "<h1>Header</h1>"
    assert(extractBody(Converter.convertDocument(source)) == expected)
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

  test("converts unordered list") {
    val source = "* Item 1\n* Item 2"
    val expected = "<ul>\n<li>\nItem 1\n</li>\n<li>\nItem 2\n</li>\n</ul>"
    assert(extractBody(Converter.convertDocument(source)) == expected)
  }

  test("converts ordered list") {
    val source = "1. Item 1\n2. Item 2"
    val expected = "<ol>\n<li>\nItem 1\n</li>\n<li>\nItem 2\n</li>\n</ol>"
    assert(extractBody(Converter.convertDocument(source)) == expected)
  }

  test("isListItem") {
    assert(Converter.isListItem("* Aaa"))
    assert(Converter.isListItem(" * Aaa"))
    assert(Converter.isListItem(" + Aaa"))
    assert(Converter.isListItem("- Aaa"))
    assert(Converter.isListItem("10. Aaa"))
    assert(!Converter.isListItem("Aaa"))
    assert(!Converter.isListItem("10.Aaa"))
    assert(!Converter.isListItem("*Aaa"))
    assert(!Converter.isListItem("** Aaa"))
  }

  test("isTableSeparator") {
    assert(Converter.isTableSeparator("|---|"))
    assert(Converter.isTableSeparator("|---|---|"))
    assert(Converter.isTableSeparator("|:---:|---:|:---|---|"))
    assert(!Converter.isTableSeparator("---"))
    assert(!Converter.isTableSeparator("|--|"))
  }

  test("escaping") {
    assert(Converter.escapeHtml("< > &") == "&lt; &gt; &amp;")
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
