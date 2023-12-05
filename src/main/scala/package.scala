import ReconstCadenas._
import ReconstCadenasPar._
import Oraculo._
import org.scalameter._

package object MedirTiempos {

  def CompararTiempos(f1: (Int, Oraculo) => Unit, f2: (Int, Oraculo) => Unit, size: Int, oracle: Oraculo): (Double, Double, Double) = {
    val seqTime = config().withWarmer(new Warmer.Default).measure(f1(size, oracle))
    val parTime = config().withWarmer(new Warmer.Default).measure(f2(size, oracle))
    val speedup = seqTime.value / parTime.value
    (seqTime.value, parTime.value, speedup)
  }

  def CompararTiemposV2(f1: (Int, Oraculo) => Unit, f2: (Int, Oraculo) => Unit, size: Int, oracle: Oraculo): (Double, Double, Double) = {
    val seqTime = config(
      Key.exec.minWarmupRuns := 5,
      Key.exec.benchRuns := 5,
      Key.verbose := false
    ).withWarmer(new Warmer.Default).measure(f1(size, oracle))

    val parTime = config(
      Key.exec.minWarmupRuns := 5,
      Key.exec.benchRuns := 5,
      Key.verbose := false
    ).withWarmer(new Warmer.Default).measure(f2(size, oracle))

    val speedup = seqTime.value / parTime.value
    (seqTime.value, parTime.value, speedup)
  }
 /* type Algoritmo = () => Unit

  def medirTiempoEjecucion(algoritmo: Algoritmo): Double = {
    config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.exec.benchRuns -> 120),
      KeyValue(Key.verbose -> true)
    ) withWarmer {
      new Warmer.Default
    } measure {
      algoritmo()
    }
  }

  def compararTiempoEjecucion(algoritmo1: Algoritmo, algoritmo2: Algoritmo): (Double, Double, Double) = {
    val tiempoEjecucion1 = medirTiempoEjecucion(algoritmo1)
    val tiempoEjecucion2 = medirTiempoEjecucion(algoritmo2)

    println(s"Tiempo de ejecución del primer algoritmo: $tiempoEjecucion1")
    println(s"Tiempo de ejecución del segundo algoritmo: $tiempoEjecucion2")

    val speedUp = tiempoEjecucion1 / tiempoEjecucion2
    (tiempoEjecucion1, tiempoEjecucion2, speedUp)
  }*/

}
