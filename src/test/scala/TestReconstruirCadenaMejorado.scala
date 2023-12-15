import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaMejorado extends AnyFunSuite{
  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaMejorado") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'a', 'a')
    val resultado = ReconstruirCadenaMejorado(4, oraculo)
    assert(resultado == Seq('a', 'a', 'a', 'a'))
  }
  test("TestReconstruirCadenaMejorado2") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 'g', 'a')
    val resultado = ReconstruirCadenaMejorado(4, oraculo)
    assert(resultado == Seq('a', 'g', 'g', 'a'))
  }
  test("TestReconstruirCadenaMejorado3") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('t', 'g', 'c')
    val resultado = ReconstruirCadenaMejorado(3, oraculo)
    assert(resultado == Seq('t', 'g', 'c'))
  }
  test("TestReconstruirCadenaMejorado4") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'x', 'b', 'a')
    val resultado = ReconstruirCadenaMejorado(3, oraculo)
    assert(resultado.isEmpty)
  }


}
