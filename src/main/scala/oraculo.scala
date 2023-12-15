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

  def SubSecuencias(secuencia: Seq[Char]): (Seq[Char], Seq[Char]) = {
    val mitad = secuencia.length / 2
    (secuencia.take(mitad), secuencia.drop(mitad))
  }

  // Nuevo método mejorarIngenuo
  // Método mejorarIngenuo
  def ReconstruirCadenaMejorao(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliar(secuencia: Seq[Char], n: Int): Seq[Char] = {
      if (n == 0) {
        secuencia
      } else {
        val (izq, der) = SubSecuencias(secuencia)
        val izq2 = reconstruirCadenaIngenuo(n/2, izq => o(izq))
        val der2 = reconstruirCadenaIngenuo(n, der => o(der))
        izq2 ++ der2
      }
    }

    auxiliar(Seq(), n)
  }


  def main(args: Array[String]): Unit = {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'g', 't', 'c')

    // Probamos la función reconstruirCadenaIngenuo con n = 4 y nuestro oráculo
    val resultadoIngenuo = reconstruirCadenaIngenuo(4, oraculo)
    // Imprimimos el resultado
    println("Resultado Ingenuo: " + resultadoIngenuo)

    // Probamos la función mejorarIngenuo con n = 4 y nuestro oráculo
    val resultadoMejorado =  ReconstruirCadenaMejorao(4, oraculo)
    // Imprimimos el resultado
    println("Resultado Mejorado: " + resultadoMejorado)


  }
}
