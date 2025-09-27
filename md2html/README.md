# Markdown to HTML converter

This is a simple program that converts file in Markdown format to HTML.

This is a learning exercise, so it doesn't support a lot of advanced features and doesn't handle many corner cases.

* Convert single file: `sbt "run [file_name].md"`. The result will be written at the same location with .html extension.
* Convert all .md files in a directory:  `sbt "run [dir_name]"`.
* Run tests: `sbt run`