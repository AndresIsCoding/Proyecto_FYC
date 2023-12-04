package object ReconstCadenasPar
{
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Función auxiliar para generar todas las posibles secuencias de longitud n en paralelo
    def generarSecuenciasParalelo(n: Int, secuenciaActual: Seq[Char]): Seq[Seq[Char]] = {
      // Guarda la longitud original de la secuencia
      val longitud = n
      // Si n es 0, devuelve la secuencia actual
      if (n == 0) Seq(secuenciaActual)
      else {
        // Si n es menor o igual a la longitud original menos el umbral, genera las secuencias en paralelo
        if (n <= longitud - umbral) {
          // Divide el alfabeto en grupos de 4 letras
          val alfabetoPartes = alfabeto.grouped(4).toList

          // Genera secuencias en paralelo para cada grupo de letras
          val resultados = parallel(
            parallel(
              generarSecuenciasParalelo(n - 1, secuenciaActual :+ alfabetoPartes(0)(0)),
              generarSecuenciasParalelo(n - 1, secuenciaActual :+ alfabetoPartes(0)(1))
            ),
            parallel(
              generarSecuenciasParalelo(n - 1, secuenciaActual :+ alfabetoPartes(0)(2)),
              generarSecuenciasParalelo(n - 1, secuenciaActual :+ alfabetoPartes(0)(3))
            )
          )
          // Combina los resultados de todos los grupos
          resultados._1._1 ++ resultados._1._2 ++ resultados._2._1 ++ resultados._2._2
        } else {
          // Si n es mayor que la longitud original menos el umbral, genera las secuencias de forma secuencial
          for {
            letra <- alfabeto
            secuencia <- generarSecuenciasParalelo(n - 1, secuenciaActual :+ letra)
          } yield secuencia
        }
      }
    }

    // Genera todas las posibles secuencias de longitud n
    val todasLasSecuencias = generarSecuenciasParalelo(n, Seq())
    // Usa el oráculo para verificar cada secuencia y encontrar la primera que pertenece a S
    // Si no se encuentra ninguna, devuelve una secuencia vacía
    if (todasLasSecuencias.indexWhere(o) == -1) Seq()
    else todasLasSecuencias(todasLasSecuencias.indexWhere(o))
   }
----------------------------------------------------------------------------------------------------------------------------------------------------------------
}
