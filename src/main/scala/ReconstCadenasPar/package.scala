package object ReconstCadenasPar
{
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val longitud = n + 1
    // Función auxiliar para generar todas las posibles secuencias de longitud n en paralelo
    def generarSecuenciasParalelo(n: Int, secuenciasActuales: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      // Guarda la longitud original de la secuencia

      //println(s"Valor de n: $n")
      //println(n <= longitud - umbral)

      // Si n es 1, devuelve las secuencias actuales
      if (n == 1) secuenciasActuales
      else {
        // Si n es mayor o igual al umbral, genera las secuencias en paralelo
        if (n <= longitud - umbral) {

          // Divide el alfabeto en grupos de 4 letras
          val alfabetoPartes = alfabeto.grouped(4).toList

          // Genera secuencias en paralelo para cada grupo de letras
          val resultados = parallel(
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)),
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)),
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)),
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(3))
          )
          
          // Combina los resultados de todos los grupos
          val nuevasSecuencias = resultados._1 ++ resultados._2 ++ resultados._3 ++ resultados._4

          // Llama a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        } else {

          // Si n es menor que el umbral, genera las secuencias de forma secuencial
          val nuevasSecuencias = for {
            secuenciaActual <- secuenciasActuales

            letra <- alfabeto
            nuevaSecuencia = secuenciaActual :+ letra

          }
          yield nuevaSecuencia

          // Llamamos a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        }
      }
    }

    // Genera todas las posibles secuencias de longitud n
    val todasLasSecuencias = generarSecuenciasParalelo(n, alfabeto.map(Seq(_)))

    // Usa el oráculo para verificar cada secuencia y encontrar la primera que pertenece a S
    // Si no se encuentra ninguna, devuelve una secuencia vacía
    if (todasLasSecuencias.indexWhere(o) == -1) Seq()
    else todasLasSecuencias(todasLasSecuencias.indexWhere(o))
  }
//----------------------------------------------------------------------------------------------------------------------------------------------------------------

    
    def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))
    val longitud = n+1

    // Función auxiliar para generar todas las posibles secuencias de longitud n en paralelo
    def generarSecuenciasParalelo(n: Int, secuenciasActuales: Seq[Seq[Char]]): Seq[Seq[Char]] = {

      // Si n es 1, devuelve las secuencias actuales
      if (n == 1) secuenciasActuales
      else {

        // Si n es menor o igual al umbral y la secuencia no es generada por una única letra, genera las secuencias en paralelo
        if (n <= longitud - umbral && !(alfabeto_filtrado.length == 1)) {

          // Divide el alfabeto filtrado en grupos de 4 letras
          val alfabetoPartes = alfabeto_filtrado.grouped(alfabeto_filtrado.length).toList
          println(s"AlfabetoPartes: $alfabetoPartes")

          // Utiliza un reconocimiento de patrones para procesar cada parte del alfabeto en paralelo según su longitud
          val resultados = alfabeto_filtrado.length match {
            case 2 =>
              // Agrega la primera letra del alfabeto filtrado a cada secuencia actual y filtra las secuencias candidatas
              val (res1, res2) = parallel(
                secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)).filter(o),
                secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)).filter(o)
              )
              // Devuelve una tupla con los resultados y dos listas vacías
              (res1, res2, List(), List())

            case 3 =>
              // Agrega la primera letra del alfabeto filtrado a cada secuencia actual y filtra las secuencias candidatas
              val ((res1, res2), res3) = parallel(
                parallel( secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)).filter(o),
                          secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)).filter(o)),
                          secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)).filter(o)
              )
              // Devuelve una tupla con los resultados y una lista vacía
              (res1, res2, res3, List())

            case _ =>
              // Agrega la primera letra del alfabeto filtrado a cada secuencia actual y filtra las secuencias candidatas
              val ((res1, res2), (res3, res4)) = parallel(
                parallel(
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)).filter(o),
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)).filter(o)),
                parallel(
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)).filter(o),
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(3)).filter(o))
              )
              // Devuelve una tupla con los resultados
              (res1, res2, res3, res4)

          }
          // Descompone la tupla resultados en valores individuales
          val (res1, res2, res3, res4) = resultados

          // Combina los resultados de todos los grupos
          val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

          // Llama a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        } else {
          // Si n es menor que el umbral, genera las secuencias de forma secuencial
          val nuevasSecuencias = for {
            secuenciaActual <- secuenciasActuales
            letra <- alfabeto_filtrado

            // Agrega la letra a la secuencia actual para crear una nueva secuencia
            nuevaSecuencia = secuenciaActual :+ letra

            // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
            if o(nuevaSecuencia)
          }
          yield nuevaSecuencia // Devuelve la nueva secuencia

          // Llamamos a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        }
      }
    }

    // Generar todas las posibles secuencias de longitud n con valores
    val todasLasSecuencias = generarSecuenciasParalelo(n, alfabeto_filtrado.map(Seq(_)))

    //Devolvemos la única secuencia candidata de tamaño n que queda luego de terminar la función recursiva
    todasLasSecuencias.head
  }

}
