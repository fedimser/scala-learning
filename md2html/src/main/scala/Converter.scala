import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.Source

object Converter {
  val HEADER_PATTERN = raw"^#{1,6} +.*".r // 1–6 #, at least one space, then anything
  val ITALIC_PATTERN = """(\*|_)(.+?)\1""".r
  val BOLD_PATTERN = """(\*\*|__)(.+?)\1""".r
  val BOLD_ITALIC_PATTERN = """(\*\*\*|___)(.+?)\1""".r
  val LINK_PATTERN = """\[(.+?)\]\((.+?)\)""".r
  val IMAGE_PATTERN = """!\[(.*?)\]\((.*?)\)""".r
  val HORIZONTAL_RULE_PATTERN = """^\s*([-*_])(\s*\1){2,}\s*$""".r
  val STRIKE_THROUGH_PATTERN = """(~~)(.+?)\1""".r
  val INLINE_CODE_PATTERN = """(`)(.+?)\1""".r
  val OL_PREFIX_PATTERN = """^\s*\d+\.\s+""".r
  val UL_PREFIX_PATTERN = """^\s*[*+-]\s+""".r
  val REFERENCE_DEFINITION_PATTERN = """\[(.+?)\]:\s*(\S+?)\s*""".r
  val TABLE_SEPARATOR_PATTERN = """\|(\s*:?-{3,}:?\s*\|)+""".r // For simplicity, require leading and trailing pipes.

  def isListItem(line: String): Boolean = {
    return OL_PREFIX_PATTERN.findPrefixOf(line).isDefined || UL_PREFIX_PATTERN.findPrefixOf(line).isDefined
  }

  def getBlockquotePrefix(line: String): String = {
    line.takeWhile(c => c == '>' || c == ' ')
  }

  def isTableSeparator(line: String): Boolean = {
    TABLE_SEPARATOR_PATTERN.matches(line)
  }

  def convertDocument(source: String): String = {
    return new Converter().convertDocument(source)
  }

  def convertInline(source: String): String = {
    return new Converter().convertInline(source)
  }
}

class Converter {
  import Converter._

  var sb: StringBuilder = new StringBuilder()
  var refMap: mutable.Map[String, String] = mutable.Map[String, String]()

  /**
   * Converts given markdown document to HTML document.
   *
   * Supported features:
   * * Basic paragraphs.
   * * Headers.
   * * Codeblocks.
   * * Horizontal rules.
   * * Blockquotes.
   * * Lists (ordered and unordered, with nesting).
   * * Tables.
   * * All the inline formatting features (see {@link convertInline}).
   */
  def convertDocument(source: String): String = {
    val lines: List[String] = source
      .linesIterator
      .toList

    sb = new StringBuilder()
    sb.append("<html>\n");
    sb.append("<head>\n<style>\n")
    sb.append(Source.fromResource("style.css").mkString)
    sb.append("</style>\n</head>\n")
    sb.append("<body>\n")


    val currentGroup = ListBuffer[String]()
    var isInsideCodeBlock = false
    var isInsideList = false

    // Writes accumulated lines to result.
    def flushCurrentGroup() = {
      if (currentGroup.nonEmpty) {
        if (isInsideList) {
          convertList(currentGroup.toList)
          isInsideList = false
        } else {
          convertLineGroup(currentGroup.toList)
        };
        currentGroup.clear()
      }
    }

    for (originalLine <- lines) {
      val line = originalLine.trim

      if (line.startsWith("```")) {
        // Beginning or end of a code block.
        if (!isInsideCodeBlock) {
          // Code block starts.
          flushCurrentGroup()
          isInsideCodeBlock = true
        } else {
          // Code block ends. Write current group to result as is.
          sb.append("<pre style='background-color: #f0f0f0; padding: 10px;'>")
          sb.append(currentGroup.mkString("\n"))
          sb.append("</pre>")
          currentGroup.clear()
          isInsideCodeBlock = false
        }
      } else if (isInsideCodeBlock) {
        currentGroup += line
      } else if (HEADER_PATTERN.matches(line)) {
        // This is a header.
        convertHeader(line);
      } else if (HORIZONTAL_RULE_PATTERN.matches(line)) {
        flushCurrentGroup()
        sb.append("<hr>\n")
      } else if (line == "") {
        // This is an empty line. Finalize previous line group.
        flushCurrentGroup()
      } else if (isInsideList) {
        currentGroup += originalLine;
      } else if (isListItem(line)) {
        flushCurrentGroup()
        currentGroup += line;
        isInsideList = true
      } else {
        // Append line to the current group.
        currentGroup += line;
      }
    }

    flushCurrentGroup()
    sb.append("</body>\n");
    sb.append("</html>\n");
    return sb.toString
  }

  def convertHeader(line: String) = {
    val level = line.takeWhile(_ == '#').length
    assert(line(level) == ' ')
    sb.append("<h" + level + ">")
    sb.append(convertInline(line.substring(level + 1)))
    sb.append("</h" + level + ">\n")
  }

  // Converts group of lines without line breaks, which is normally a paragrpah.
  def convertLineGroup(lineGroup: List[String]): Unit = {
    // If every line begins with ">", this is blockquote.
    if (lineGroup(0).startsWith(">")) {
      convertBlockQuote(lineGroup, 1)
      return
    }

    if (lineGroup.length >= 2 && isTableSeparator(lineGroup(1))) {
      renderTable(lineGroup)
      return
    }

    // For now, just join them into a single paragprah.
    sb.append("<p>\n")
    for (line <- lineGroup) {
      sb.append(convertInline(line))
      sb.append("\n")
    }
    sb.append("</p>\n")
  }

  // Converts group of lines without line breaks, known to represent a list.
  def convertList(lineGroup: List[String]): Unit = {
    val isUnordered = UL_PREFIX_PATTERN.findPrefixOf(lineGroup(0)).isDefined
    val isOrdered = OL_PREFIX_PATTERN.findPrefixOf(lineGroup(0)).isDefined
    assert(isUnordered || isOrdered)
    val tag = if (isUnordered) "ul" else "ol"
    val prefixPattern = if (isUnordered) UL_PREFIX_PATTERN else OL_PREFIX_PATTERN
    sb.append("<" + tag + ">\n")

    var linesForCurrentItem = ListBuffer[String]()

    def finalizeListItem(): Unit = {
      if (linesForCurrentItem.isEmpty) {
        return
      }
      val n = linesForCurrentItem.takeWhile(l => !isListItem(l)).size
      val (linesForItem, linesForNestedList) = linesForCurrentItem.toList.splitAt(n)
      sb.append("<li>\n")
      for (line <- linesForItem) {
        sb.append(convertInline(line))
        sb.append("\n")
      }
      sb.append("</li>\n")
      if (linesForNestedList.nonEmpty) {
        convertList(linesForNestedList)
      }
      linesForCurrentItem.clear()
    }

    var curIndent = 1000
    for (line <- lineGroup) {
      val numLeadingSpaces = line.takeWhile(c => c == ' ').size
      val prefix = prefixPattern.findFirstIn(line)
      if (prefix.isDefined && numLeadingSpaces < curIndent) {
        // This is the start of next item.
        finalizeListItem()
        curIndent = prefix.get.length
        linesForCurrentItem += line.substring(curIndent)
      } else {
        // This is continuation of current item.
        linesForCurrentItem += line.substring(Math.min(numLeadingSpaces, curIndent))
      }
    }
    finalizeListItem()

    sb.append("</" + tag + ">\n")
  }

  // Converts group of lines known to be a blockquote.
  def convertBlockQuote(lineGroup: List[String], level: Integer): Unit = {
    sb.append(s"<blockquote>\n")

    val numLinesAtThisLevel: Int = lineGroup.takeWhile(line => getBlockquotePrefix(line).count(_ == '>') <= level).size
    val (linesAtThisLevel, linesAtNextLevel) = lineGroup.splitAt(numLinesAtThisLevel)
    for (line <- linesAtThisLevel) {
      val quote_prefix = getBlockquotePrefix(line)
      sb.append(convertInline(line.substring(quote_prefix.length)))
      sb.append("\n")
    }

    if (linesAtNextLevel.nonEmpty) {
      convertBlockQuote(linesAtNextLevel, level + 1)
    }

    sb.append("</blockquote>\n")
  }

  /**
   * Converts a single line from markdown to HTML.
   *
   * This function only handles inline formatting features.
   *
   * Supported features:
   * * Emphasis (italic, bold, bold italic).
   * * Hyperlinks (including reference-style links).
   * * Images.
   * * Strikethrough text.
   * * Inline code.
   */
  def convertInline(source: String): String = {
    var s = source
    s = BOLD_ITALIC_PATTERN.replaceAllIn(s, m => s"<strong><em>${m.group(2)}</em></strong>")
    s = BOLD_PATTERN.replaceAllIn(s, m => s"<strong>${m.group(2)}</strong>")
    s = ITALIC_PATTERN.replaceAllIn(s, m => s"<em>${m.group(2)}</em>")
    s = IMAGE_PATTERN.replaceAllIn(s, m => s"""<img src="${m.group(2)}" alt="${m.group(1)}"/>""")
    s = LINK_PATTERN.replaceAllIn(s, m => s"""<a href="${m.group(2)}">${m.group(1)}</a>""")
    s = STRIKE_THROUGH_PATTERN.replaceAllIn(s, m => s"<del>${m.group(2)}</del>")
    s = INLINE_CODE_PATTERN.replaceAllIn(s, m => s"<code>${m.group(2)}</code>")
    return s
  };

  // Column styles for current table.
  private var colStyles: Array[String] = Array()

  def renderTable(lineGroup: List[String]): Unit = {
    assert(lineGroup.size >= 2)
    assert(isTableSeparator(lineGroup(1)))

    // Infer alignment.
    colStyles = lineGroup(1).split("\\|").drop(1).map(_.trim).map(x => columnSpecToStyle(x))

    sb.append("<table>\n")
    renderTableRow(lineGroup(0), "th")
    for (i <- 2 until lineGroup.length) {
      renderTableRow(lineGroup(i), "td")
    }
    sb.append("</table>\n")
  }

  def renderTableRow(line: String, cellTag: String) = {
    sb.append("<tr>\n")
    val cells = line.split("(?<!\\\\)\\|").map(_.replaceAllLiterally("\\|", "|")).drop(1)
    for (i <- 0 until Math.min(colStyles.size, cells.size)) {
      sb.append(s"<$cellTag${colStyles(i)}>")
      sb.append(convertInline(cells(i)))
      sb.append(s"</$cellTag>\n")
    }
    sb.append("</tr>\n")
  }

  def columnSpecToStyle(spec: String): String = {
    if (spec.startsWith(":") && spec.endsWith(":")) {
      return " style='text-align: center;'"
    } else if (spec.startsWith(":")) {
      return " style='text-align: left;'"
    } else if (spec.endsWith(":")) {
      return " style='text-align: right;'"
    } else {
      return ""
    }
  }
}

