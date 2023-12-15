import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaMejoradoParalelo extends AnyFunSuite {
  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaIngenuoParallel") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't')
    val secuencia = ReconstruirCadenaMejoradoPar(2)(4, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't'))
  }
  //crea test tamaño 8
  test("TestReconstruirCadenaIngenuoParallel2") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val secuencia = ReconstruirCadenaMejoradoPar(2)(8, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }
  //crea test tamaño 12
  test("TestReconstruirCadenaIngenuoParallel3") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val secuencia = ReconstruirCadenaMejoradoPar(2)(12, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }
//crea test tamaño devuelve lista vacia
  test("TestReconstruirCadenaIngenuoParallel4") {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'e', 't', 'f')
    val secuencia = ReconstruirCadenaMejoradoPar(2)(4, oraculo)
    assert(secuencia.isEmpty)
  }



}
