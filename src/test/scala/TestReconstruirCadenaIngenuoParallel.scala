import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaIngenuoParallel extends AnyFunSuite{
  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaIngenuoParallel") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 'a', 'a')
    val resultado = reconstruirCadenaIngenuoParallel(4, oraculo)
    assert(resultado == Seq('a', 'a', 'a', 'a'))
  }
  test("TestReconstruirCadenaIngenuoParallel2") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 'g', 'a')
    val resultado = reconstruirCadenaIngenuoParallel(4, oraculo)
    assert(resultado == Seq('a', 'g', 'g', 'a'))
  }
  test("TestReconstruirCadenaIngenuoParallel3") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('t', 'g', 'c')
    val resultado = reconstruirCadenaIngenuoParallel(3, oraculo)
    assert(resultado == Seq('t', 'g', 'c'))
  }
  test("TestReconstruirCadenaIngenuoParallel4") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'x', 'b', 'a')
    val resultado = reconstruirCadenaIngenuoParallel(3, oraculo)
    assert(resultado.isEmpty)
  }
  //crear test para hacer preguntas al oraculo tamaño 12
test("TestReconstruirCadenaIngenuoParallel5") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'a', 't', 'g', 'g', 'a', 'a', 'a', 't', 'a','a')
    val resultado = reconstruirCadenaIngenuoParallel(12, oraculo)
    assert(resultado == Seq('a', 'c', 'a', 't', 'g', 'g', 'a', 'a', 'a', 't', 'a','a'))
  }


}
