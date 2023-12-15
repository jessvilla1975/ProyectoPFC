
import common._
import scala.collection.parallel.CollectionConverters._
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.annotation.tailrec
object oraculo {
  // Definir el alfabeto
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Definir el tipo Oraculo como una función
  type Oraculo = Seq[Char] => Boolean

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliar(secuencia: Seq[Char], n: Int): Seq[Char] = {
      if (n == 0) {
        secuencia
      } else {
        alfabeto.map(letra => auxiliar(secuencia :+ letra, n - 1)).find(o).getOrElse(Seq())
      }
    }
    auxiliar(Seq(), n)
  }

  // Nuevo método mejorarIngenuo
  // Método mejorarIngenuo
  def mejorarIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliar(secuencia: Seq[Char], n: Int): Seq[Char] = {
      if (n == 0) {
        secuencia
      } else {
        val mitad = secuencia.length / 2
        val s1 = auxiliar(secuencia.take(mitad), mitad)
        val s2 = auxiliar(secuencia.drop(mitad), n - mitad - 1)
        // Preguntar al oráculo si s1 y s2 son subcadenas de la secuencia
        //llamar al metodo reconstrutirCadenaIngenuo con s1 y s2
        val s3 = reconstruirCadenaIngenuo(n, o)
        //concatenar s1, s2 y s3
        s1 ++ s2 ++ s3

      }
    }

    auxiliar(Seq(), n)
  }


  def main(args: Array[String]): Unit = {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 't', 'a')

    // Probamos la función reconstruirCadenaIngenuo con n = 4 y nuestro oráculo
    val resultadoIngenuo = reconstruirCadenaIngenuo(4, oraculo)
    // Imprimimos el resultado
    println("Resultado Ingenuo: " + resultadoIngenuo)

    // Probamos la función mejorarIngenuo con n = 4 y nuestro oráculo
    val resultadoMejorado = mejorarIngenuo(4, oraculo)
    // Imprimimos el resultado
    println("Resultado Mejorado: " + resultadoMejorado)
  }
}
