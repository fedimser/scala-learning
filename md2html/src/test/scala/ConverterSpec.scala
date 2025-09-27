import org.scalatest.funsuite.AnyFunSuite

class ConverterSpec extends AnyFunSuite {
  test("temporary test") {
    assert(Converter.convert("a") == "<html>a</html>")
  }

}
