import scala.collection.mutable.ListBuffer

object Converter {
  val HEADER_PATTERN = raw"^#{1,6} +.*".r // 1–6 #, at least one space, then anything
  val ITALIC_PATTERN = """(\*|_)(.+?)\1""".r
  val BOLD_PATTERN = """(\*\*|__)(.+?)\1""".r
  val BOLD_ITALIC_PATTERN = """(\*\*\*|___)(.+?)\1""".r
  val LINK_PATTERN = """\[(.+?)\]\((.+?)\)""".r
  val IMAGE_PATTERN = """!\[(.*?)\]\((.*?)\)""".r
  val HORIZONTAL_RULE_PATTERN = """^\s*([-*_])(\s*\1){2,}\s*$""".r
  val STRIKE_THROUGH_PATTERN = """(~~)(.+?)\1""".r

  val BLOCKQUOTE_STYLE = "border-left:4px solid #888; background:rgba(0,0,0,0.05); padding:0.75em 1em; margin:1em 0;"

  /**
   * Converts given markdown document to HTML document.
   *
   * Supported features:
   * * Basic paragraphs.
   * * Headers.
   * * Codeblocks.
   * * Horizontal rules.
   * * Blockquotes.
   * * All the inline formatting features (see {@link convertInline}).
   * TODO:
   * * Lists (ordered and unordered).
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
        convertHeader(sb, line);
      } else if (HORIZONTAL_RULE_PATTERN.matches(line)) {
        flushCurrentGroup()
        sb.append("<hr>\n")
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

  // Converts group of lines without line breaks, which is normally a paragrpah.
  def convertLineGroup(sb: StringBuilder, lineGroup: List[String]) : Unit = {
    // If every line begins with ">", this is blockquote.
    if (lineGroup(0).startsWith(">")) {
      convertBlockQuote(sb, lineGroup, 1)
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

  // Converts group of lines known to be a blockquote.
  def convertBlockQuote(sb: StringBuilder, lineGroup: List[String], level: Integer) : Unit = {
    sb.append(s"<blockquote style='$BLOCKQUOTE_STYLE'>\n")

    val numLinesAtThisLevel: Int = lineGroup.takeWhile(line => getBlockquotePrefix(line).count(_ == '>') <= level).size
    val (linesAtThisLevel, linesAtNextLevel) = lineGroup.splitAt(numLinesAtThisLevel)
    for (line <- linesAtThisLevel) {
      val quote_prefix = getBlockquotePrefix(line)
      sb.append(convertInline(line.substring(quote_prefix.length)))
      sb.append("\n")
    }

    if (linesAtNextLevel.nonEmpty) {
      convertBlockQuote(sb, linesAtNextLevel, level + 1)
    }

    sb.append("</blockquote>\n")
  }

  def getBlockquotePrefix(line: String): String = {
    line.takeWhile(c => c == '>' || c == ' ')
  }

  /**
   * Converts a single line from markdown to HTML.
   *
   * This function only handles inline formatting features.
   *
   * Supported features:
   * * Emphasis (italic, bold, bold italic).
   * * Hyperlinks.
   * * Images.
   * * Strikethrough text.
   * TODO:
   * * Inline code.
   * * Reference-style links.
   */
  def convertInline(source: String): String = {
    var s = source
    s = BOLD_ITALIC_PATTERN.replaceAllIn(s, m => s"<strong><em>${m.group(2)}</em></strong>")
    s = BOLD_PATTERN.replaceAllIn(s, m => s"<strong>${m.group(2)}</strong>")
    s = ITALIC_PATTERN.replaceAllIn(s, m => s"<em>${m.group(2)}</em>")
    s = IMAGE_PATTERN.replaceAllIn(s, m => s"""<img src="${m.group(2)}" alt="${m.group(1)}"/>""")
    s = LINK_PATTERN.replaceAllIn(s, m => s"""<a href="${m.group(2)}">${m.group(1)}</a>""")
    s = STRIKE_THROUGH_PATTERN.replaceAllIn(s, m => s"<del>${m.group(2)}</del>")
    return s
  };

}