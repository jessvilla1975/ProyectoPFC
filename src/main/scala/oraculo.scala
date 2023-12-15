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
      if (n == 0) { // Si n es 0, devolvemos la secuencia
        secuencia
      } else {
        // Si n no es 0, añadimos cada letra del alfabeto a la secuencia y llamamos recursivamente a la función
        alfabeto.map(letra => auxiliar(secuencia :+ letra, n - 1)).find(o).getOrElse(Seq())
      }
    }
    auxiliar(Seq(), n) // Llamamos a la función auxiliar con una secuencia vacía y n
  }


  def ReconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliar(secuencia: Seq[Char], k: Int): Seq[Char] = {
      if (k == n && o(secuencia)) { // Si la secuencia es de tamaño n y es aceptada por el oráculo, devolvemos la secuencia
        secuencia
      } else if (k < n) { // Si la secuencia es de tamaño menor que n, la dividimos en dos y llamamos recursivamente a la función
         alfabeto.map(letra => auxiliar(secuencia :+ letra, k + 1)).find(o).getOrElse(Seq())
      } else {
        Seq()
      }
    }
    auxiliar(Seq(), 0)
  }

  def main(args: Array[String]): Unit = {
    val oraculo: Oraculo = (s: Seq[Char]) => s == Seq('a', 'a', 't', 'c')

    // Acumular resultados
    var resultadoIngenuo: Seq[Char] = null
    var resultadoMejorado: Seq[Char] = null

    // Medir tiempo de ejecución para reconstruirCadenaIngenuo
    val tiempoIngenuo = withWarmer(new Warmer.Default) measure {
      resultadoIngenuo = reconstruirCadenaIngenuo(4, oraculo)
    }

    // Medir tiempo de ejecución para ReconstruirCadenaMejorado
    val tiempoMejorado = withWarmer(new Warmer.Default) measure {
      resultadoMejorado = ReconstruirCadenaMejorado(4, oraculo)
    }

    // Imprimir resultados una sola vez al final
    println("Resultado Ingenuo: " + resultadoIngenuo)
    println(s"Tiempo de ejecución para reconstruirCadenaIngenuo: $tiempoIngenuo ")

    println("Resultado Mejorado: " + resultadoMejorado)
    println(s"Tiempo de ejecución para ReconstruirCadenaMejorado: $tiempoMejorado ")
  }
}
