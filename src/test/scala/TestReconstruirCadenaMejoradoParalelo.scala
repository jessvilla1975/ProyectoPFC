import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaMejoradoParalelo extends AnyFunSuite {
  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaMejoradoParalelo") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'a', 'a')
    val resultado = ReconstruirCadenaMejoradoParalelo(4, oraculo)
    assert(resultado == Seq('a', 'a', 'a', 'a'))
  }
  test("TestReconstruirCadenaMejoradoParalelo2") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 'g', 'a')
    val resultado = ReconstruirCadenaMejoradoParalelo(4, oraculo)
    assert(resultado == Seq('a', 'g', 'g', 'a'))
  }
  test("TestReconstruirCadenaMejoradoParalelo3") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('t', 'g', 'c')
    val resultado = ReconstruirCadenaMejoradoParalelo(3, oraculo)
    assert(resultado == Seq('t', 'g', 'c'))
  }
  test("TestReconstruirCadenaMejoradoParalelo4") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'x', 'b', 'a')
    val resultado = ReconstruirCadenaMejoradoParalelo(3, oraculo)
    assert(resultado.isEmpty)
  }
  //crear test para hacer preguntas al oraculo tamaño 12
  test("TestReconstruirCadenaMejoradoParalelo5") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'a', 't', 'g', 'g', 'a', 'a', 'a', 't', 'a', 'a')
    val resultado = ReconstruirCadenaMejoradoParalelo(12, oraculo)
    assert(resultado == Seq('a', 'c', 'a', 't', 'g', 'g', 'a', 'a', 'a', 't', 'a', 'a'))
  }


}
