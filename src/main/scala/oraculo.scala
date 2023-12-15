import common._
import scala.collection.parallel.CollectionConverters._
import org.scalameter.measure
import org.scalameter._
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

object oraculo {
  // Definir el alfabeto
  val alfabeto = Seq('a', 'c', 'g', 't')
  // Definir el tipo Oraculo como una función
  type Oraculo = Seq[Char] => Boolean

  //metodo para reconstruir cadena ingenuo
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
  //metodo para reconstruir cadena ingenuo parallel
  def reconstruirCadenaIngenuoParallel(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliar(secuencia: Seq[Char], n: Int): Seq[Char] = {
      if (n == 0) {
        secuencia
      } else {
        if (n < umbral) {
          // Realizar llamadas de manera secuencial para tamaños pequeños
          reconstruirCadenaIngenuo(n, o)
        } else {
          // Utilizar par.map para realizar llamadas en paralelo con task
          val parallelResults = alfabeto.par.map { letra =>
            task(auxiliar(secuencia :+ letra, n - 1)).join() // Utilizar task para crear una tarea
          }
          // Utilizar find sobre la secuencia resultante de forma no paralela
          parallelResults.find(o).getOrElse(Seq())
        }
      }
    }

    auxiliar(Seq(), n)
  }

  //metodo para reconstruir cadena mejorado

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
  //metodo para reconstruir cadena mejorado parallel usando metodo parallel de common para paralelizar
  def ReconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def auxiliarParalelo(secuencia: Seq[Char], k: Int): Seq[Char] = {
      if (k == n && o(secuencia)) {
        secuencia
      } else if (k < n) {
        if (n < umbral) {
          // Realizar llamadas de manera secuencial para tamaños pequeños
          ReconstruirCadenaMejorado(n, o)
        } else {
          // Utilizar par.map para realizar llamadas en paralelo con task
          val resultadosParalelos = alfabeto.par.map { letra =>
            task(auxiliarParalelo(secuencia :+ letra, k + 1)).join() // Utilizar task para crear una tarea
          }
          // Utilizar find sobre la secuencia resultante de forma no paralela
          resultadosParalelos.find(o).getOrElse(Seq())
        }
      } else {
        Seq()
      }
    }

    auxiliarParalelo(Seq(), 0)
  }








  //metodo para reconstruir cadena turbo

  def ReconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirRec(k: Int, secuencia: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) {
        secuencia.find(_.length == n).getOrElse(Seq()) // Devolver la cadena de longitud n si existe, de lo contrario, una lista vacía
      } else {
        val nuevosCandidatos = secuencia.flatMap(candidato =>
          alfabeto.map(letra => candidato :+ letra)
        )
        if (nuevosCandidatos.isEmpty) Seq() // No hay cadenas válidas de longitud k

        reconstruirRec(k * 2, nuevosCandidatos) // Llamada recursiva con el doble de la longitud
      }
    }

    reconstruirRec(1, alfabeto.map(Seq(_)))
  }

  def ReconstruirCadenaTurboMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirRec(k: Int, secuencia: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) {
        secuencia.find(_.length == n).getOrElse(Seq()) // Devolver la cadena de longitud n si existe, de lo contrario, una lista vacía
      } else {
        val nuevosCandidatos = secuencia.flatMap(candidato =>
          alfabeto.map(letra => candidato :+ letra)
        ).filter(o)
        if (nuevosCandidatos.isEmpty) Seq() // No hay cadenas válidas de longitud k

        reconstruirRec(k * 2, nuevosCandidatos) // Llamada recursiva con el doble de la longitud
      }
    }

    reconstruirRec(1, alfabeto.map(Seq(_)))
  }

 //metodo para generar oraculo aleatorio
  def generarOraculo(longitud: Int): Seq[Char] = {
    Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
  }

  def pruebas(): Unit = {
    val tamanios = Seq(4,8,12,16,20,24,28)// Diferentes tamaños de cadena para probar

    // Imprimir encabezado de la tabla
    println(f"| Tamaño | Ingenuo (ms) | Mejorado (ms) | Turbo (ms) | TurboMejorado |(ms)Oráculo |")

    for (tamano <- tamanios) {
      val oraculo = generarOraculo(tamano)

      // Medir tiempo de ejecución para reconstruirCadenaIngenuo
      val tiempoIngenuo = withWarmer(new Warmer.Default) measure {
        val resultadoIngenuo = reconstruirCadenaIngenuo(tamano, (s: Seq[Char]) => s == oraculo)
      }

      // Medir tiempo de ejecución para ReconstruirCadenaMejorado
      val tiempoMejorado = withWarmer(new Warmer.Default) measure {
        val resultadoMejorado = ReconstruirCadenaMejorado(tamano, (s: Seq[Char]) => s == oraculo)
      }
      // Medir tiempo de ejecución para ReconstruirCadenaTurbo
      val tiempoTurbo = withWarmer(new Warmer.Default) measure {
        val resultadoTurbo = ReconstruirCadenaTurbo(tamano, (s: Seq[Char]) => s == oraculo)
      }
      // Medir tiempo de ejecución para ReconstruirCadenaTurboMejorado
      val tiempoTurboMejorado = withWarmer(new Warmer.Default) measure {
        val resultadoTurboMejorado = ReconstruirCadenaTurboMejorado(tamano, (s: Seq[Char]) => s == oraculo)
      }

      // Imprimir resultados en formato de tabla
      println(f"| $tamano%6d | ${tiempoIngenuo.value}%12.4f | ${tiempoMejorado.value}%14.4f | ${tiempoTurbo.value}%14.4f |  ${tiempoTurboMejorado.value}%14.4f |${oraculo}%10s |")
    }
  }

  def compararAlgoritmos(funcionSecuencial: (Int, Oraculo) => Seq[Char], funcionParalela: (Int, Oraculo) => Seq[Char])
                        (n: Int, o: Oraculo): (Double, Double, Double) = {

    // Secuencial
    val tiempoSecuencial = withWarmer(new Warmer.Default) measure {
      funcionSecuencial(n, o)
    }

    // Paralelo
    val tiempoParalelo = withWarmer(new Warmer.Default) measure {
      funcionParalela(n, o)
    }

    // Calcular aceleración
    val aceleracion = tiempoSecuencial.value / tiempoParalelo.value

    // Devolver resultados
    (tiempoSecuencial.value, tiempoParalelo.value, aceleracion)
  }
  //comparar ingenuo vs parallel con compararAlgoritmos
  def pruebasCompararAlgoritmosIngenuo(): Unit = {
    val tamanios = Seq(4,8,10,12,14,16,18,20) // Diferentes tamaños de cadena para probar
    //imprimir encabezado de la tabla
    println(f"| Tamaño | Ingenuo (ms) | Ingenuo Parallel (ms) | Aceleracion (ms) |Oráculo |")
    //usar metodo comparar algoritmos para comparar ingenuo vs parallel
    for (tamano <- tamanios) {
      val oraculo = generarOraculo(tamano)
      val (tiempoSecuencial, tiempoParalelo, aceleracion) = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoParallel(4))(tamano, (s: Seq[Char]) => s == oraculo)
      println(f"| $tamano%6d | ${tiempoSecuencial}%12.4f | ${tiempoParalelo}%14.4f | ${aceleracion}%14.4f |  ${oraculo}%10s |")
    }
  }

  //comparar mejorado vs parallel con compararAlgoritmos
  def pruebasCompararAlgoritmosMejorado(): Unit = {
    val tamanios = Seq(4,8,10,12,14,16,18,20) // Diferentes tamaños de cadena para probar
    //imprimir encabezado de la tabla
    println(f"| Tamaño | Mejorado (ms) | Mejorado Parallel (ms) | Aceleracion (ms) |Oráculo |")
    //usar metodo comparar algoritmos para comparar ingenuo vs parallel
    for (tamano <- tamanios) {
      val oraculo = generarOraculo(tamano)
      val (tiempoSecuencial, tiempoParalelo, aceleracion) = compararAlgoritmos(ReconstruirCadenaMejorado, ReconstruirCadenaMejoradoPar(4))(tamano, (s: Seq[Char]) => s == oraculo)
      println(f"| $tamano%6d | ${tiempoSecuencial}%12.4f | ${tiempoParalelo}%14.4f | ${aceleracion}%14.4f |  ${oraculo}%10s |")
    }
  }

    def main(args: Array[String]): Unit = {
      //medir tiempo de ejecucion de reconstruirCadenamejorado parallel
      /*val oraculo = generarOraculo(4)
      val resultado = withWarmer(new Warmer.Default) measure {
        ReconstruirCadenaTurbo(4, (s: Seq[Char]) => s == oraculo)
      }
      println(resultado, oraculo)**/
      //pruebas()
       pruebasCompararAlgoritmosIngenuo()
      //pruebasCompararAlgoritmosMejorado()



    }
}
