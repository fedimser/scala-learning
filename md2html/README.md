# Markdown to HTML converter

This is a simple program that converts file in Markdown format to HTML.

This is a learning exercise, so it doesn't support a lot of advanced features and doesn't handle many corner cases.

In case when input is not valid Markdown, it will handle it without errors,
but result might not look as nice as with other renderers.

Supported features:
   * Headers. 
   * Emphasis (italic, bold, bold italic).
   * Hyperlinks (including reference-style links).
   * Images.
   * Strikethrough text.
   * Inline code.
   * Codeblocks.
   * Horizontal rules.
   * Blockquotes.
   * Lists (ordered and unordered, with nesting).
   * Tables.

Usage:
   * Convert single file: `sbt "run [file_name].md"`. The result will be written at the same location with .html extension.
   * Convert all .md files in a directory:  `sbt "run [dir_name]"`.
   * Run tests: `sbt run`