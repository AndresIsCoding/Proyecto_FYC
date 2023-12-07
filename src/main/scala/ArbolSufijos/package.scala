package object ArbolSufijos {
  abstract class Trie

  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie

  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabeza(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, hijos) => hijos.map(t => raiz(t))
      case Hoja(c, _) => Seq(c)
    }

  }


  def buscarHijo(hijos: List[Trie], caracter: Char): Option[Trie] = {
    if (hijos.isEmpty) {
      None
    } else {
      val head = hijos.head
      if (raiz(head) == caracter) {
        Some(head)
      } else {
        buscarHijo(hijos.tail, caracter)
      }
    }
  }

  def perteneceCadena(cadena: String, trie: Trie): Boolean = {
    // Función interna recursiva para verificar la pertenencia
    def perteneceRec(c: String, t: Trie): Boolean = {
      c match {
        case "" =>
          t match {
            case Nodo(_, marcada, _) => marcada // Verificar el valor booleano si termina en un nodo
            case Hoja(_, marcada) => marcada
            case _ => false // No debería llegar aquí
          }

        case _ =>
          t match {
            case Nodo(_, _, hijos) =>
              val primerCaracter = c.head
              val restoCadena = c.tail

              // Buscar el nodo correspondiente al primer carácter en los hijos
              val hijoEncontrado = buscarHijo(hijos, primerCaracter)

              hijoEncontrado match {
                case Some(nodo) => perteneceRec(restoCadena, nodo)
                case None => false // No se encontró el carácter en los hijos
              }

            case _ => false // No se puede continuar la búsqueda en una hoja
          }
      }
    }

    perteneceRec(cadena, trie)
  }


  def adicionar(s: Seq[Char], trie: Trie): Trie = {

    def adicionarRecursivo(s: Seq[Char], trie: Trie): Trie = {
      val Nodo(car, bool, lista) = trie


      (s.tail.isEmpty, cabeza(trie).containsSlice(Seq(s.head))) match {
        case (true, false) => Nodo(car, bool, lista ++ List(Hoja(s.head, true)))
        case (true, true) => Nodo(car, bool, lista.filter(x => !compare(x, s.head)) ++
          List(End(lista.filter(x => compare(x, s.head)).head)))
        case (false, true) => Nodo(car, bool, List(adicionarRecursivo(s.tail, toNodo(lista.filter(x => compare(x, s.head)).head))) ++
          lista.filter(x => !compare(x, s.head)))
        case (false, false) => Nodo(car, bool, lista ++ List(adicionarRecursivo(s.tail, Nodo(s.head, false, List()))))
      }

    }

    def toNodo(trie: Trie): Trie = {
      val Nodo(car, bool, lista) = trie match {
        case Nodo(car, bool, lista) => Nodo(car, bool, lista)
        case Hoja(car, bool) => Nodo(car, bool, List())
      }
      Nodo(car, bool, lista)
    }

    def End(trie: Trie): Trie = {
      val Nodo(car, bool, lista) = trie
      Nodo(car, true, lista)
    }

    def compare(trie: Trie, ref: Char): Boolean = {
      val Nodo(car, bool, lista) = trie match {
        case Nodo(car, bool, lista) => Nodo(car, bool, lista)
        case Hoja(car, bool) => Nodo(car, bool, List())
      }

      if (car == ref) true
      else false
    }

    adicionarRecursivo(s, trie)
  }

  def construirTrie(ss: Seq[Seq[Char]]): Trie = {
    def construirRecursivo(ss: Seq[Seq[Char]], trie: Trie): Trie = {
      ss match {
        case Nil => trie
        case head :: tail =>
          val nuevoTrie = adicionar(head, trie)
          construirRecursivo(tail, nuevoTrie)
      }
    }

    construirRecursivo(ss, Nodo('_', false, List()))
  }
}
