import Oraculo.{Oraculo, alfabeto}

package object ReconstCadenas
{
     def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    // Función auxiliar para generar todas las posibles secuencias de longitud n
    def generarSecuencias(n: Int, secuenciasActuales: Seq[Seq[Char]]): Seq[Seq[Char]] = {

      // Caso base de la recursión: si n es 1, devuelve las secuencias actuales
      if (n == 1) secuenciasActuales
      else {
        //println("Llamado")
        // Caso recursivo: para cada secuencia actual y cada letra en el alfabeto, genera todas las secuencias posibles
        // añadiendo la letra a la secuencia actual y disminuyendo n en 1
        val nuevasSecuencias = for {
          secuenciaActual <- secuenciasActuales

          letra <- alfabeto
          nuevaSecuencia = secuenciaActual :+ letra

        }
        yield nuevaSecuencia

        // Llamamos a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
        generarSecuencias(n - 1, nuevasSecuencias)
      }
    }

    // Generar todas las posibles secuencias de longitud n
    val todasLasSecuencias = generarSecuencias(n, alfabeto.map(Seq(_)))

    // Usar el oráculo para verificar cada secuencia y encontrar la primera que pertenece a S
    // Si no se encuentra ninguna, devuelve una secuencia vacía
    //println(s"Todas las secuencias: $todasLasSecuencias")
    if (todasLasSecuencias.indexWhere(o) == -1) Seq()
    else todasLasSecuencias(todasLasSecuencias.indexWhere(o))
  }

//----------------------------------------------------------------------------------------------------------------------------------------------------------

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Función auxiliar para generar todas las posibles secuencias de longitud n
    def generarSecuencias(n: Int, secuenciasActuales: Seq[Seq[Char]]): Seq[Seq[Char]] = {

      // Caso base de la recursión: si n es 1, devuelve las secuencias actuales
      if (n == 1) secuenciasActuales
      else {
        // Caso recursivo: para cada secuencia actual y cada letra en el alfabeto, genera todas las secuencias posibles
        // añadiendo la letra a la secuencia actual y disminuyendo n en 1
        val nuevasSecuencias = for {
          secuenciaActual <- secuenciasActuales
          //_ = println(s"Secuencia candidata: $secuenciaActual")
          letra <- alfabeto_filtrado
          nuevaSecuencia = secuenciaActual :+ letra
          //_ = println(s"Secuencia actual: $nuevaSecuencia")

          // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
          if o(nuevaSecuencia)
        }
        yield nuevaSecuencia

        // Llamamos a la función recursivamente con las nuevas secuencias y disminuyendo n en 1
        generarSecuencias(n - 1, nuevasSecuencias)
      }
    }

    // Generar todas las posibles secuencias de longitud n con valores repetidos
    val todasLasSecuencias = generarSecuencias(n, alfabeto_filtrado.map(Seq(_)))

    //Devolvemos la única secuencia candidata de tamaño n que queda luego de terminar la función recursiva
    todasLasSecuencias.head
  }

  //----------------------------------------------------------------------------------------------------------------------------------------------------------

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {

    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Calculamos el logaritmo base 2 de n
    val logN = (Math.log(n) / Math.log(2)).toInt

    // Función recursiva para generar todas las subsecuencias de tamaño n
    def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

      // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
      if (potencia > logN) secuencias
      else {
       // println("======================================================================================================")
        // Generamos nuevas secuencias concatenando todas las secuencias existentes entre sí
        val nuevasSecuencias = for {
          secuencia1 <- secuencias
          secuencia2 <- secuencias
          nuevaSecuencia = secuencia1 ++ secuencia2

          // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
        // _ = println(s"Consultando al oráculo: $nuevaSecuencia")
          if o(nuevaSecuencia)
        }
        yield nuevaSecuencia

       // println(s"Potencia $potencia: Subecuencias candidatas: $nuevasSecuencias")

        // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
        generarSubsecuencias(nuevasSecuencias, potencia + 1)
      }
    }

    // Creamos las secuencias iniciales con cada letra del alfabeto que hemos filtrado
    val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))

    // Generamos todas las subsecuencias de tamaño n
    val todasLasSubsecuencias = generarSubsecuencias(secuenciasIniciales, 1)

    //Devolvemos la única secuencia candidata de tamaño n que queda luego de terminar la función recursiva
    todasLasSubsecuencias.head
  }

  //----------------------------------------------------------------------------------------------------------------------------------------------------------

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Calculamos el logaritmo base 2 de n
    val logN = (Math.log(n) / Math.log(2)).toInt

    // Función recursiva para generar todas las subsecuencias de tamaño n
    def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

      // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
      if (potencia > logN) secuencias
      else {
        //println(s"Total secuencias candidatas: ${secuencias.length}")
        // Generamos nuevas secuencias concatenando todas las secuencias existentes entre sí
        val nuevasSecuencias = for {
          secuencia1 <- secuencias
          secuencia2 <- secuencias
          nuevaSecuencia = secuencia1 ++ secuencia2

          segundoCuarto = nuevaSecuencia.slice(nuevaSecuencia.length/4, nuevaSecuencia.length/2)
          tercerCuarto = nuevaSecuencia.slice(nuevaSecuencia.length/2, 3*nuevaSecuencia.length/4)
          // Solo continúa con la recursión si la subsecuencia formada por el segundo y tercer cuarto de la nueva secuencia es candidata
          if secuencias.contains(segundoCuarto ++ tercerCuarto)

          // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
         // _ = println(s"Consultando al oráculo: $nuevaSecuencia")
          if o(nuevaSecuencia)
        }
        yield  nuevaSecuencia

        //println(s"Potencia $potencia: Subecuencias candidatas: $nuevasSecuencias")

        // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
        generarSubsecuencias(nuevasSecuencias, potencia + 1)
      }
    }

    // Creamos las secuencias iniciales con cada letra del alfabeto que hemos filtrado
    val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))
   // println(s"Secuencias iniciales $secuenciasIniciales")

    // Generamos todas las subsecuencias de tamaño n
    val todasLasSubsecuencias = generarSubsecuencias(secuenciasIniciales, 1)

    //Devolvemos la única secuencia candidata de tamaño n que queda luego de terminar la función recursiva
    todasLasSubsecuencias.head
  }
//**************************************************************************************************************************


  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    // Filtrar el alfabeto para incluir solo las letras que son subcadenas candidatas
    val alfabeto_filtrado = alfabeto.filter(letra => o(Seq(letra)))

    // Calculamos el logaritmo base 2 de n
    val logN = (Math.log(n) / Math.log(2)).toInt

    def generarSubsecuencias(secuencias: Seq[Seq[Char]], potencia: Int): Seq[Seq[Char]] = {

      // Si hemos sobrepasado el potencia logN, devolvemos las secuencias que tenemos
      if (potencia > logN) secuencias
      else {
        //println("======================================================================================================")
        // Generamos nuevas secuencias concatenando todas las secuencias existentes entre sí
        val nuevasSecuencias = for {
          secuencia1 <- secuencias
          secuencia2 <- secuencias
          nuevaSecuencia = secuencia1 ++ secuencia2

          // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
          //_ = println(s"Consultando al oráculo: $nuevaSecuencia")
          if o(nuevaSecuencia)
        }
        yield nuevaSecuencia

        //println(s"Potencia $potencia: Subecuencias candidatas: $nuevasSecuencias")

        // Llamamos a la función recursivamente con las nuevas secuencias y aumentamos la potencia
        generarSubsecuencias(nuevasSecuencias, potencia + 1)
      }
    }

    // Creamos las secuencias iniciales con cada letra del alfabeto que hemos filtrado
    val secuenciasIniciales = alfabeto_filtrado.map(Seq(_))
    //println(s"Secuencias iniciales $secuenciasIniciales")

    val trie = construirTrie(generarSubsecuencias(secuenciasIniciales, 1))

    println(trie)

    def trieToSeqChar(trie: Trie): Seq[Char] = {

      def dfs(t: Trie, prefix: Seq[Char]): Seq[Char] = {
        t match {
          case Nodo(_, _, hijos) =>
            hijos.flatMap(h => dfs(h, prefix :+ raiz(h)))

          case Hoja(car, _) =>
            prefix :+ car
        }
      }

      dfs(trie, Seq())
    }


    trieToSeqChar(trie).init
  }

}
