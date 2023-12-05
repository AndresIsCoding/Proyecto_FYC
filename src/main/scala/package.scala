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

  //Cualquiera de las 2 funciona , en teoría la segunda es ligeramente más rápida
  
//Recibe 2 funciones, el tamaño de la secuencia a calcular y el oráculo correspondiente a esa secuencia.
  //Devuelve, el teimpo de ambas funciones y la aceleración de la segunda respecto a la primera
  
  //Ejemplo de uso
  /*CompararTiempos(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaPar(0),sec3.length,or_3)
  CompararTiemposV2(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaPar(0),sec3.length,or_3)*/
}
