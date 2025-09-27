import scala.collection.mutable.ListBuffer

object Converter {
  val HEADER_PATTERN = raw"^#{1,6} +.*".r // 1–6 #, at least one space, then anything
  val ITALIC_PATTERN = """(\*|_)(.+?)\1""".r
  val BOLD_PATTERN = """(\*\*|__)(.+?)\1""".r
  val BOLD_ITALIC_PATTERN = """(\*\*\*|___)(.+?)\1""".r
  val LINK_PATTERN = """\[(.+?)\]\((.+?)\)""".r

  /**
   * Converts given markdown document to HTML document.
   *
   * Supported features:
   * * Basic paragraphs.
   * * Headers.
   * * Codeblocks.
   * * All the inline formatting features (see {@link convertInline}).
   * TODO:
   * * Lists (ordered and unordered).
   * * Horizontal rules.
   * * Tables.
   */
  def convertDocument(source: String): String = {
    val lines: List[String] = source
      .linesIterator
      .map(_.trim)
      .toList

    val sb = new StringBuilder("<html>\n");
    val currentGroup = ListBuffer[String]()
    var isInsideCodeBlock = false

    // Writes accumulated lines to result.
    def flushCurrentGroup() = {
      if (currentGroup.nonEmpty) {
        convertLineGroup(sb, currentGroup.toList);
        currentGroup.clear()
      }
    }

    for (line <- lines) {
      if (line.startsWith("```"))  {
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
        convertHeader(sb, line);
      } else if (line == "") {
        // This is an empty line. Finalize previous line group.
        flushCurrentGroup()
      } else {
        // Append line to the current group.
        currentGroup += line;
      }
    }

    flushCurrentGroup()
    sb.append("</html>\n");
    return sb.toString
  }

  def convertHeader(sb: StringBuilder, line: String) = {
    val level = line.takeWhile(_ == '#').length
    assert(line(level) == ' ')
    sb.append("<h" + level + ">")
    sb.append(convertInline(line.substring(level + 1)))
    sb.append("</h" + level + ">\n")
  }

  def convertLineGroup(sb: StringBuilder, lineGroup: List[String]) = {
    // For now, just join them into a single paragprah.
    sb.append("<p>\n")
    for (line <- lineGroup) {
      sb.append(convertInline(line))
      sb.append("\n")
    }
    sb.append("</p>\n")
  }

  /**
   * Converts a single line from markdown to HTML.
   *
   * This function only handles inline formatting features.
   *
   * Supported features:
   * * Emphasis (italic, bold, bold italic).
   * * Hyperlinks.
   * TODO:
   * * Lists (ordered and unordered).
   * * Images.
   * * Blockquotes.
   * * Inline code.
   * * Strikethrough text.
   */
  def convertInline(source: String): String = {
    var s = source
    s = BOLD_ITALIC_PATTERN.replaceAllIn(s, m => s"<strong><em>${m.group(2)}</em></strong>")
    s = BOLD_PATTERN.replaceAllIn(s, m => s"<strong>${m.group(2)}</strong>")
    s = ITALIC_PATTERN.replaceAllIn(s, m => s"<em>${m.group(2)}</em>")
    s = LINK_PATTERN.replaceAllIn(s, m => s"""<a href="${m.group(2)}">${m.group(1)}</a>""")
    return s
  };

}