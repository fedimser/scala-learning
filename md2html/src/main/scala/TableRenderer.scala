class TableRenderer(sb: StringBuilder) {

  private var colStyles: Array[String] = Array()

  def renderTable(lineGroup: List[String]): Unit = {
    assert(lineGroup.size >= 2)
    assert(TableRenderer.isTableSeparator(lineGroup(1)))

    // Infer alignment.
    colStyles = lineGroup(1).split("\\|").drop(1).map(_.trim).map(x=>TableRenderer.columnSpecToStyle(x))

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
      sb.append(Converter.convertInline(cells(i)))
      sb.append(s"</$cellTag>\n")
    }
    sb.append("</tr>\n")
  }
}

object TableRenderer {
  // For simplicity, require leading and trailing pipes.
  val TABLE_SEPARATOR_PATTERN = """\|(\s*:?-{3,}:?\s*\|)+""".r

  def isTableSeparator(line: String): Boolean = {
    TABLE_SEPARATOR_PATTERN.matches(line)
  }

  def columnSpecToStyle(spec: String) : String = {
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
