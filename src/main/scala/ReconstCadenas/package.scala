import Oraculo.{Oraculo, alfabeto}

package object ReconstCadenas
{
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Función auxiliar para generar todas las posibles secuencias de longitud n
    def generarSecuencias(n: Int, secuenciaActual: Seq[Char]): Seq[Seq[Char]] = {
      // Caso base de la recursión: si n es 0, devuelve la secuencia actual
      if (n == 0) {
        Seq(secuenciaActual)
      } else {
        for {
          // Caso recursivo: para cada letra en el alfabeto, genera todas las secuencias posibles
          // añadiendo la letra a la secuencia actual y disminuyendo n en 1
          letra <- alfabeto
          secuencia <- generarSecuencias(n - 1, secuenciaActual :+ letra)
        } yield secuencia
      }
    }

    // Generar todas las posibles secuencias de longitud n con valores repetidos
    val todasLasSecuencias = generarSecuencias(n, Seq())

    // Usar el oráculo para verificar cada secuencia y encontrar la primera que pertenece a S
    // Si no se encuentra ninguna, devuelve una secuencia vacía
    todasLasSecuencias.find(o).getOrElse(Seq())
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
  }
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Función auxiliar para generar todas las posibles secuencias de longitud n
    def generarSecuencias(n: Int, secuenciaActual: Seq[Char]): Seq[Seq[Char]] = {

      // Caso base de la recursión: si n es 0, devuelve la secuencia actual
      if (n == 0) {
        Seq(secuenciaActual)
      } else {
        // Caso recursivo: para cada letra en el alfabeto, genera todas las secuencias posibles
        // añadiendo la letra a la secuencia actual y disminuyendo n en 1
        for {
          letra <- alfabeto
          nuevaSecuencia = secuenciaActual :+ letra
          // Solo continúa con la recursión si la nueva secuencia es una subcadena candidata
          if o(nuevaSecuencia)
          secuencia <- generarSecuencias(n - 1, nuevaSecuencia)
        } yield secuencia
      }
    }

    // Generar todas las posibles secuencias de longitud n con valores repetidos
    val todasLasSecuencias = generarSecuencias(n, Seq())

    // Usar el oráculo para verificar cada secuencia y encontrar la primera que pertenece a S
    // Si no se encuentra ninguna, devuelve una secuencia vacía
    todasLasSecuencias.find(o).getOrElse(Seq())
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
  }

  def reconstruirCadenaTurbo (n: Int, o: Oraculo):Seq[Char]=
  {
    // Función auxiliar para generar todas las posibles secuencias de longitud n
    def generarSecuencias(n: Int, secuenciaActual: Seq[Char]): Seq[Seq[Char]] = {
      // Caso base de la recursión: si n es 0, devuelve la secuencia actual
      if (n == 0) {
        Seq(secuenciaActual)
      } else {
        for {
          // Caso recursivo: para cada letra en el alfabeto, genera todas las secuencias posibles
          // añadiendo la letra a la secuencia actual y disminuyendo n en 1
          letra <- alfabeto
          secuencia <- generarSecuencias(n - 1, secuenciaActual :+ letra)
        } yield secuencia
      }
    }

  }

}
