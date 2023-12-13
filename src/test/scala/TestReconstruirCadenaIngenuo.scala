import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaIngenuo extends AnyFunSuite {

  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaIngenuo") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'a', 'a')
    val resultado = reconstruirCadenaIngenuo(4, oraculo)
    assert(resultado == Seq('a', 'a', 'a', 'a'))
  }
  test("TestReconstruirCadenaIngenuo2") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 'g', 'a')
    val resultado = reconstruirCadenaIngenuo(4, oraculo)
    assert(resultado == Seq('a', 'g', 'g', 'a'))
  }
  test("TestReconstruirCadenaIngenuo3") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('t', 'g', 'c')
    val resultado = reconstruirCadenaIngenuo(3, oraculo)
    assert(resultado == Seq('t', 'g', 'c'))
  }
  test("TestReconstruirCadenaIngenuo4") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'x', 'b', 'a')
    val resultado = reconstruirCadenaIngenuo(3, oraculo)
    assert(resultado.isEmpty)
  }





}
