import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.immutable.Seq
import oraculo._
@RunWith(classOf[JUnitRunner])
class TestReconstruirCadenaIngenuoParallel extends AnyFunSuite{
  //crear test para hacer preguntas al oraculo
  test("TestReconstruirCadenaIngenuoParallel"){
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't')
    val secuencia = reconstruirCadenaIngenuoParallel(2)(4, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't'))
  }
  //crea test tamaño 8
  test("TestReconstruirCadenaIngenuoParallel2"){
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val secuencia = reconstruirCadenaIngenuoParallel(4)(8, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }
  //crea test tamaño 12
  test("TestReconstruirCadenaIngenuoParallel3"){
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val secuencia = reconstruirCadenaIngenuoParallel(8)(12, oraculo)
    assert(secuencia == Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }



}
