import scala.collection.mutable.ListBuffer

object Converter {
  val HEADER_PATTERN = raw"^#{1,6} +.*".r // 1–6 #, at least one space, then anything

  /**
   * Converts given markdown document to HTML document.
   *
   * Supported features:
   * * Basic paragraphs.
   * * Headers.
   * * All the inline formatting features (see {@link convertInline}).
   * TODO:
   * * Lists (ordered and unordered).
   * * Codeblocks.
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

    for (line <- lines :+ "") {
      if (HEADER_PATTERN.matches(line)) {
        // This is a header.
        convertHeader(sb, line);
      } else if (line == "") {
        // This is an empty line. Finalize previous line group.
        if (currentGroup.nonEmpty) {
          convertLineGroup(sb, currentGroup.toList);
          currentGroup.clear()
        }
      } else {
        // Append line to the current group.
        currentGroup += line;
      }
    }

    sb.append("</html>\n");
    return sb.toString
  }

  def convertHeader(sb: StringBuilder, line: String) = {
    val level = line.takeWhile(_ == '#').length
    assert(line(level) == ' ')
    sb.append("<h" + level + ">")
    convertInline(sb, line.substring(level + 1))
    sb.append("</h" + level + ">\n")
  }

  def convertLineGroup(sb: StringBuilder, lineGroup: List[String]) = {
    // For now, just join them into a single paragprah.
    sb.append("<p>\n")
    for (line <- lineGroup) {
      convertInline(sb, line);
      sb.append("\n");
    }
    sb.append("</p>\n");
  }

  /**
   * Converts a single line from markdown to HTML.
   *
   * This function only handles inline formatting features.
   *
   * Supported features:
   * * None.
   * TODO:
   * * Emphasis (italic, bold, bold italic).
   * * Lists (ordered and unordered).
   * * Hyperlinks.
   * * Images.
   * * Blockquotes.
   * * Inline code.
   * * Strikethrough text.
   */
  def convertInline(sb: StringBuilder, source: String) = {
    sb.append(source);
  };

}