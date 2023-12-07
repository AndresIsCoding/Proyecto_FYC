import common._
import Oraculo._
import ArbolSufijos._

package object ReconstCadenasPar
{
   def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val longitud = n+1
    // Función auxiliar para generar todas las posibles secuencias de longitud n en paralelo
    def generarSecuenciasParalelo(n: Int, secuenciasActuales: Seq[Seq[Char]]): Seq[Seq[Char]] = {

      // Si n es 1, devuelve las secuencias actuales
      if (n == 1) secuenciasActuales
      else {
        // Si n es mayor o igual al umbral, genera las secuencias en paralelo
        if (n <= longitud - umbral) {

          // Divide el alfabeto en grupos de 4 letras
          val alfabetoPartes = alfabeto.grouped(4).toList

          // Genera secuencias en paralelo para cada grupo de letras
          val resultados = parallel(
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)), //Solo añade la letra a
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)), //Solo añade la letra c
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)), //Solo añade la letra g
            secuenciasActuales.map(_ :+ alfabetoPartes(0)(3))  //Solo añade la letra t
          )

          // Combina los resultados de todos los grupos
          val nuevasSecuencias = resultados._1 ++ resultados._2 ++ resultados._3 ++ resultados._4


          // Llama a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        } else {
          //println("Llamado secuencial")

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
    //println(s"Todas las secuencias: $todasLasSecuencias")
    if (todasLasSecuencias.indexWhere(o) == -1) Seq()
    else todasLasSecuencias(todasLasSecuencias.indexWhere(o))
  }
//-------------------------------------------------------------------------------------------------------------------------------------

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
          //println("Llamado paralelo")

          // Divide el alfabeto filtrado en grupos de letras
          val alfabetoPartes = alfabeto_filtrado.grouped(alfabeto_filtrado.length).toList
          //println(s"AlfabetoPartes: $alfabetoPartes")

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
                parallel(
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)).filter(o),
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)).filter(o)
                ),
                secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)).filter(o)
              )
              // Devuelve una tupla con los resultados y una lista vacía
              (res1, res2, res3, List())

            case _ =>
              // Agrega la primera letra del alfabeto filtrado a cada secuencia actual y filtra las secuencias candidatas
              val ((res1, res2), (res3, res4)) = parallel(
                parallel(
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(0)).filter(o),
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(1)).filter(o)
                ),
                parallel(
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(2)).filter(o),
                  secuenciasActuales.map(_ :+ alfabetoPartes(0)(3)).filter(o)
                )
              )
              // Devuelve una tupla con los resultados
              (res1, res2, res3, res4)

          }

          // Descompone la tupla resultados en valores individuales
          val (res1, res2, res3, res4) = resultados
          //println(s"res 1: $res1")
          //println(s"res 2: $res2")
          //println(s"res 3: $res3")
          //println(s"res 4: $res4")
          //println(s"Resultados: $resultados")

          // Combina los resultados de todos los grupos
          val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

          // Llama a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
          generarSecuenciasParalelo(n - 1, nuevasSecuencias)
        } else {
          //println("Llamado secuencial")
          // Si n es menor que el umbral, genera las secuencias de forma secuencial
          val nuevasSecuencias = for {
            secuenciaActual <- secuenciasActuales
            //_ = println(s"Secuencia candidata: $secuenciaActual")
            letra <- alfabeto_filtrado

            // Agrega la letra a la secuencia actual para crear una nueva secuencia
            nuevaSecuencia = secuenciaActual :+ letra
            //_ = println(s"Secuencia actual: $nuevaSecuencia")

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

//-------------------------------------------------------------------------------------------------------------------------------------

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Calculamos el logaritmo base 2 de n
    val logN = (Math.log(n) / Math.log(2)).toInt

    // Función recursiva para generar todas las subsecuencias de tamaño n
    def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

      // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
      if (potencia > logN) secuencias
      else {
        // Si la potencia es mayor o igual al umbral y la longitud de las secuencias es mayor a 16, genera las subsecuencias en paralelo
        if (potencia >= umbral && secuencias.length > 16) {

          // Divide las secuencias en cuatro partes
          val tamañoGrupo = secuencias.length / 4
          val secuenciasPartes = secuencias.grouped(tamañoGrupo).toList

          // Si la longitud de la lista no es un múltiplo de 4, añade las secuencias restantes al último grupo
          if (secuencias.length % 4 != 0) {
            val secuenciasRestantes = secuencias.drop(tamañoGrupo * 4)

            // Procesa cada parte de las secuencias en paralelo
            val (res1, res2, res3, res4) = parallel(
              secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
              secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
              secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
              (secuenciasPartes(3) ++ secuenciasRestantes).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o)
            )
            // Combina los resultados de todos los grupos
            val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

            // Llama a la función recursivamente con las nuevas secuencias y aumenta la potencia
            generarSubsecuencias(nuevasSecuencias, potencia + 1)
          } else {
          // Si la longitud de la lista  es un múltiplo de 4 cada grupo procesará 1/4 de secuencia

          // Procesa cada parte de las secuencias en paralelo
          val (res1, res2, res3, res4) = parallel(
            secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
            secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
            secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o),
            secuenciasPartes(3).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(o)
          )

          // Combina los resultados de todos los grupos
          val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4
         //println(s"Secuencias Candidatas: ${nuevasSecuencias}")

          // Llama a la función recursivamente con las nuevas secuencias y aumenta la potencia
          generarSubsecuencias(nuevasSecuencias, potencia + 1)}
        } else {
          // Si la potencia es menor que el umbral o la longitud de las secuencias es menor que 16, genera las subsecuencias de forma secuencial
          val nuevasSecuencias = for {
            secuencia1 <- secuencias
            secuencia2 <- secuencias
            nuevaSecuencia = secuencia1 ++ secuencia2
            if o(nuevaSecuencia)
          } yield nuevaSecuencia

          // Llama a la función recursivamente con las nuevas secuencias y aumenta la potencia
          generarSubsecuencias(nuevasSecuencias, potencia + 1)
        }
      }
    }

    // Crea las secuencias iniciales con cada letra del alfabeto filtrado
    val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))

    // Genera todas las subsecuencias de tamaño n
    val todasLasSubsecuencias = generarSubsecuencias(secuenciasIniciales, 1)

    // Devuelve la única secuencia candidata de tamaño n que queda luego de terminar la función recursiva
    todasLasSubsecuencias.head
  }
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
        // Filtramos el alfabeto para incluir solo las letras que son subcadenas candidatas
        val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

        // Calculamos el logaritmo base 2 de n
        val logN = (Math.log(n) / Math.log(2)).toInt

        // Definimos una función recursiva para generar todas las subsecuencias de tamaño n
        def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

          // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
          if (potencia > logN) secuencias
          else {
            // Si la potencia es mayor o igual al umbral y el tamaño de las secuencias es mayor a 16, entonces paralelizamos la generación de subsecuencias
            if (potencia >= umbral && secuencias.length > 16) {

              //println("Llamado paralelo")
              // Dividimos las secuencias en grupos de tamaño tamañoGrupo
              val tamañoGrupo = secuencias.length / 4
              val secuenciasPartes = secuencias.grouped(tamañoGrupo).toList

              //println(s"Total secuencias candidatas: ${secuencias.length}")
              //println(s"Secuencias Partes: $secuenciasPartes")

              // Si la longitud de la lista no es un múltiplo de 4, añade las secuencias restantes al último grupo
              if (secuencias.length % 4 != 0) {
                val secuenciasRestantes = secuencias.drop(tamañoGrupo * 4)

                // Procesa cada parte de las secuencias en paralelo
                val (res1, res2, res3, res4) = parallel(
                  secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo
                    // y tercer cuarto de la nueva secuencia esté contenida en las secuencias
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  // Repetimos el mismo proceso para las otras partes de las secuencias
                  secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  (secuenciasPartes(3) ++ secuenciasRestantes).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  })
                )
                // Combina los resultados de todos los grupos
                val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

                // Llama a la función recursivamente con las nuevas secuencias y aumenta la potencia
                generarSubsecuencias(nuevasSecuencias, potencia + 1)
              } else {
                // Paralelizamos la generación de nuevas secuencias y la consulta al oráculo
                val (res1, res2, res3, res4) = parallel(
                  secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo
                    // y tercer cuarto de la nueva secuencia esté contenida en las secuencias
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  // Repetimos el mismo proceso para las otras partes de las secuencias
                  secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  }),
                  secuenciasPartes(3).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                    val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                    val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                    secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
                  })
                )

                // Concatenamos los resultados de las cuatro tareas paralelas
                val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

                // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
                generarSubsecuencias(nuevasSecuencias, potencia + 1)
              }
            } else {
              //println("Llamado secuencial")
              //println(s"Total secuencias candidatas: ${secuencias.length}")
              // Si la potencia es menor que el umbral o el tamaño de las secuencias es menor o igual a 16, entonces generamos las subsecuencias de forma secuencial
              val nuevasSecuencias = for {
                secuencia1 <- secuencias
                secuencia2 <- secuencias
                nuevaSecuencia = secuencia1 ++ secuencia2
                // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo y
                // tercer cuarto de la nueva secuencia esté contenida en las secuencias
                segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length/4, nuevaSecuencia.length/2)
                tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length/2, 3*nuevaSecuencia.length/4)
                if secuencias.contains(segundoCuarto ++ tercerCuarto)
                if o(nuevaSecuencia)
              } yield nuevaSecuencia

              // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
              generarSubsecuencias(nuevasSecuencias, potencia + 1)
            }
          }
        }

        // Creamos las secuencias iniciales con cada letra del alfabeto que hemos filtrado
        val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))

        // Generamos todas las subsecuencias
        val todasLasSubsecuencias = generarSubsecuencias(secuenciasIniciales, 1)

        // Devolvemos la primera subsecuencia
        //println(todasLasSubsecuencias)
        todasLasSubsecuencias.head
  }

  //-------------------------------------------------------------------------------------------------------
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Filtramos el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Calculamos el logaritmo base 2 de n
    val logN = (Math.log(n) / Math.log(2)).toInt

    // Definimos una función recursiva para generar todas las subsecuencias de tamaño n
    def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

      // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
      if (potencia > logN) secuencias
      else {
        // Si la potencia es mayor o igual al umbral y el tamaño de las secuencias es mayor a 16, entonces paralelizamos la generación de subsecuencias
        if (potencia >= umbral && secuencias.length > 16) {

          // Dividimos las secuencias en grupos de tamaño tamañoGrupo
          val tamañoGrupo = secuencias.length / 4
          val secuenciasPartes = secuencias.grouped(tamañoGrupo).toList

          // Si la longitud de la lista no es un múltiplo de 4, añade las secuencias restantes al último grupo
          if (secuencias.length % 4 != 0) {
            val secuenciasRestantes = secuencias.drop(tamañoGrupo * 4)

            // Procesa cada parte de las secuencias en paralelo
            val (res1, res2, res3, res4) = parallel(
              secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo
                // y tercer cuarto de la nueva secuencia esté contenida en las secuencias
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              // Repetimos el mismo proceso para las otras partes de las secuencias
              secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              (secuenciasPartes(3) ++ secuenciasRestantes).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              })
            )
            // Combina los resultados de todos los grupos
            val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

            // Llama a la función recursivamente con las nuevas secuencias y aumenta la potencia
            generarSubsecuencias(nuevasSecuencias, potencia + 1)
          } else {
            // Paralelizamos la generación de nuevas secuencias y la consulta al oráculo
            val (res1, res2, res3, res4) = parallel(
              secuenciasPartes(0).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo
                // y tercer cuarto de la nueva secuencia esté contenida en las secuencias
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              // Repetimos el mismo proceso para las otras partes de las secuencias
              secuenciasPartes(1).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              secuenciasPartes(2).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              }),
              secuenciasPartes(3).flatMap(secuencia1 => secuencias.map(secuencia2 => (secuencia1 ++ secuencia2))).filter(nuevaSecuencia => {
                val segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
                val tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
                secuencias.contains(segundoCuarto ++ tercerCuarto) && o(nuevaSecuencia)
              })
            )

            // Concatenamos los resultados de las cuatro tareas paralelas
            val nuevasSecuencias = res1 ++ res2 ++ res3 ++ res4

            // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
            generarSubsecuencias(nuevasSecuencias, potencia + 1)
          }
        } else {
          // Si la potencia es menor que el umbral o el tamaño de las secuencias es menor o igual a 16, entonces generamos las subsecuencias de forma secuencial
          val nuevasSecuencias = for {
            secuencia1 <- secuencias
            secuencia2 <- secuencias
            nuevaSecuencia = secuencia1 ++ secuencia2
            // Antes de consultar al oráculo, verificamos que la subsecuencia formada por el segundo y
            // tercer cuarto de la nueva secuencia esté contenida en las secuencias
            segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 4, nuevaSecuencia.length / 2)
            tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length / 2, 3 * nuevaSecuencia.length / 4)
            if secuencias.contains(segundoCuarto ++ tercerCuarto)
            if o(nuevaSecuencia)
          } yield nuevaSecuencia

          // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
          generarSubsecuencias(nuevasSecuencias, potencia + 1)
        }
      }
    }

    // Creamos las secuencias iniciales con cada letra del alfabeto que hemos filtrado
    val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))
    //println(s"Secuencias iniciales $secuenciasIniciales")

    //Creación el arbol
    val trie = construirTrie(generarSubsecuencias(secuenciasIniciales, 1))

    def trieToSeqCharPar(trie: Trie): Seq[Char] = {
      def dfs(t: Trie, prefix: Seq[Char]): Seq[Char] = {
        t match {
          case Nodo(_, _, hijos) =>
            hijos.flatMap(h => dfs(h, prefix :+ raiz(h)))

          case Hoja(car, _) =>
            prefix :+ car
        }
      }

      val half = dfs(trie, Seq()).length / 2

      val (dfs1, dfs2) = parallel(dfs(trie, Seq()).splitAt(half)._1, dfs(trie, Seq()).splitAt(half)._2)

      dfs1 ++ dfs2
    }
    trieToSeqCharPar(trie).init
  }
}
