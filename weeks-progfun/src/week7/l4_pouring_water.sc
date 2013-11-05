package week7

//objetivo dados unos vasos, un grifo y un sumidero y las acciones de llenado en el grifo, vaciado en el sumidero y verter agua de un vaso a otro
// dejar en uno de los vasos una cantidad (target) de agua determinada

object l4_pouring_water {

  type GlassCapacity = Int // entero que repesenta la capacidad del vaso en dcl
  type Glass = Int //Posicion del vaso en el vector de stados y capacidades (de 0 a capacties.lenght - 1)
  type States = Vector[GlassCapacity] // situacion actual de los vasos con el contenido que tienen dentro
  type Capacities = Vector[GlassCapacity] // capacidad de los vasos es fija para cada vaso

  //NOTA: NO ESTA UTILIZANDO funciones, las pone como clases heredadas de un trait
  /* //Movimientos
  // llena el vaso del grifo
  def fill(glass: Glass) = ???
  // vacion el vaso en el sumidero
  def empty(glass: Glass) = ???
  //poour vierte el contenido del vaso from en el vaso to
  def pour(from: Glass, to: Glass) = ???
   */

  // paths: conjunto con las listas todos los posibles movimientos hasta dejar un vaso con la cantidad deseada : Target
  // cuando dentro de paths encontremos una lista de movimientos que como resultado dejen un vaso con cantidad deseada de agua paramos
  // puede darse el caso que exploremos todos los path y no encontremos solución

  // creamos una clase pouring con las capacidades iniciales de los vasos
  class Pouring(capacities: Capacities) {

    // 1.- colocar a cero la cantidad inicial en los vasos (states = 0 para todos los vasos)
    // ojo estamos ante vectores inmutables por eso a esta variable la podemos llamari initialStates en vez de states, pq habra nuevos vectores con los nuevos estados
    val initialStates: States = capacities map (x => 0) // creamos un nuevo vector que sabemos tiene la longitud de capacities y lo ponemos todo a cero

    //Movimientos
    trait Move { // trato Move del que extenderan (heredaran) case class de los movimientos ¿¿Por qué??? ver l2_handling_nested_sequences week6
      //            pq lo que queremos es una lista de todos los movimientos posibles para la lista de vasos que tenemos ver (*) mas abajo

      //al principio no estaba implementada ver (**) abajo el porque se implementa
      def changeState(state: States): States
    }
    // llena el vaso del grifo
    case class fill(glass: Glass) extends Move {
      //devuelve un NUEVO vector con el estado del vaso glass a su capacidad maxima (**) al principio changeState no estaba implementada IDEM para empty y pour
      def changeState(states: States) = states updated (glass, capacities(glass))
    }
    // vacio el vaso en el sumidero
    case class empty(glass: Glass) extends Move {
      def changeState(states: States) = states updated (glass, 0) //nuevo vector con el estado del vaso fijado como vacio
    }
    //poour vierte el contenido del vaso from en el vaso to
    case class pour(from: Glass, to: Glass) extends Move {
      def changeState(states: States) = {
        // saber la cantidad que vamos a mover de from a to
        val amount = states(from) min (capacities(to) - states(to)) // como mucho puedo meter lo que falta para llenar to, pero si tengo menos en from echo lo que tenga en from
        states updated (from, states(from) - amount) updated (to, states(to) + amount) //actualizamos ambos estados de los vasos en uno quitamos la cantidad que podemos y en el otro la aumentamos
      }

    }

    //(*) lista de todos los posibles movimientos con el número de  vasos que tenemos
    val glasses = 0 until capacities.length //rango de todos los vasos que tenemos

    val fillGlasses = for (glassToFill <- glasses) yield fill(glassToFill)
    val emptyGlasses = for (glassToEmpty <- glasses) yield empty(glassToEmpty)
    val pourGlasses = for (fromGlass <- glasses; toGlass <- glasses if (fromGlass != toGlass)) yield pour(fromGlass, toGlass)
    // todos los posibles movimientos
    val moves = fillGlasses ++ emptyGlasses ++ pourGlasses
    //con todos los posibles movimientos vamos a ver los cambios desde el estado inicial
    // para ello implementamos el metodo change para los distintos movimientos ver (**)

    //una vez que hemos implementado changeState calculamos
    //PATHS (lista con las listas todos los posibles movimientos (historias) hasta dejar un vaso con la cantidad deseada : Target)
    // la define tambien una clase que tratará una historia de movimientos (una lista de movimientos)
    // el ultimo movimiento realizado lo va a poner primero en la lista
    class Path(history: List[Move]) {
      //queremos saber para la lista de movimientos (history) los estados finales de los vasos (endStates)
      // creamos una funcion recursiva que recorra la lista de movimientos aplicando los cambios hasta el final para ver en que estado
      // han quedado los vasos, la llamamos trackStates
      def endStates: States = trackStates(history)
      private def trackStates(history: List[Move]): States = history match { //la define como privada
        case Nil => initialStates // si la lista de movimientos esta vacia, devolvemos el estado inicial
        case move :: xs => move changeState (trackStates(xs)) // move es el ultimo movimiento
      }

      //otra forma de hacerlo mas elegante: con foldRight
      def endStates1: States = (history foldRight (initialStates))(_ changeState _)
      //                                                          ^recorre todos los elementos de history (movimientos empezando por el final)
      //                                                                        ^ y les cambia el estado acumulando el estado acumulando aqui el estado
      //def endStates2: States = (history foldRight(initialStates))(mv => mv.changeState) jodida la conversion Mirar con calma anagrams.sc 6-forcomp substract
      // mirar tb l2_handling_nested_sequences.sc week 6

      // otra funcion interesante para path es añadir un movimiento a la lista:
      def addMove(move: Move) = new Path(move :: history)
      // y por ultimo imprimir el path (en orden inverso y con el estado final al que se ha llegado)
      override def toString = (history.reverse mkString " ") + "--> " + endStates
    }
    
    //path inicial:
    val initPath = new Path (Nil)
 		
 		//una vez definida la clase path hay que buscar todos los paths que nos lleven a un estado final con un vaso con la cantidad adecuada
 		// hacemos una funcion similar a la de los naturales:
 		def fromNats(n: Int): Stream[Int] = n #:: fromNats(n + 1)
  // todos lo numeros naturales
  	val nats = fromNats(0)
  		
  	// no parte de un path si no de un conjunto de paths y devuelve un stream con todos los posibles paths a partir de los dados
  	// como eso es la polla no los calcula hasta no ser necesario por eso devuelve Streams
  	//def from (path: Path): Set[Path] = ???
  	def from (paths: Set[Path]): Stream [Set[Path]] = {
  		if (paths.isEmpty) Stream.Empty
  		else {// dos pasos
  			//1.- generar todos los posibles paths
  			val newPaths = for {
  				path <- paths
  				newPath <- moves map path.addMove // mapeo los posibles movimientos los añado a addMove (obtengo un path)
  			} yield newPath //entrego un set de paths
  			
  			//2.- añadirlos al stream de conjuntos de path
				paths #:: from (newPaths) //exactamente igual que from nats (el n+1) aqui se consigue iterando sobre paths dentro del for
  		}
  	}
  	// todos los conjuntos de caminos posibles
  	val pathsSet = from (Set(initPath))
  	
  	//devolvemos un (stream lista no computada hasta que se necesite) de todos los path que den como estado final un vaso
  	//con la cantidad de agua indicada en target
  	// para ello recorremos todos los conjuntos de paths y nos quedmos con los paths que son soluciones
  	def solution (target: Int): Stream[Path] = for {
  		pathSet <- pathsSet
  		path <- pathSet
  		if (path.endStates contains target)
  	} yield path
  }

  val capacities = Vector(4, 7)                   //> capacities  : scala.collection.immutable.Vector[Int] = Vector(4, 7)
  val pouring = new Pouring(capacities)           //> pouring  : week7.l4_pouring_water.Pouring = week7.l4_pouring_water$Pouring@
                                                  //| 63c4fb04
  // todos los posibles movimientos
  pouring.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with 
                                                  //| week7.l4_pouring_water.pouring.Move] = Vector(fill(0), fill(1), empty(0), e
                                                  //| mpty(1), pour(0,1), pour(1,0))
  //probamos from
  //pouring.from (Set(pouring.initPath))
  //si queremos ejecutar 3 movimientos
  pouring.pathsSet.take(3).toList                 //> res1: List[Set[week7.l4_pouring_water.pouring.Path]] = List(Set(--> Vector(
                                                  //| 0, 0)), Set(pour(0,1)--> Vector(0, 0), pour(1,0)--> Vector(0, 0), fill(0)--
                                                  //| > Vector(4, 0), fill(1)--> Vector(0, 7), empty(1)--> Vector(0, 0), empty(0)
                                                  //| --> Vector(0, 0)), Set(empty(0) pour(1,0)--> Vector(0, 0), empty(1) pour(0,
                                                  //| 1)--> Vector(0, 0), fill(0) empty(0)--> Vector(0, 0), empty(1) fill(1)--> V
                                                  //| ector(0, 7), fill(0) empty(1)--> Vector(4, 0), fill(0) pour(1,0)--> Vector(
                                                  //| 4, 0), pour(0,1) fill(1)--> Vector(0, 7), fill(1) pour(1,0)--> Vector(4, 3)
                                                  //| , pour(1,0) pour(1,0)--> Vector(0, 0), empty(1) pour(1,0)--> Vector(0, 0), 
                                                  //| fill(1) fill(1)--> Vector(0, 7), fill(0) pour(0,1)--> Vector(0, 4), pour(0,
                                                  //| 1) pour(1,0)--> Vector(0, 0), empty(0) fill(0)--> Vector(4, 0), empty(0) em
                                                  //| pty(1)--> Vector(0, 0), pour(1,0) fill(0)--> Vector(4, 0), pour(0,1) empty(
                                                  //| 1)--> Vector(0, 0), fill(0) fill(1)--> Vector(4, 7), pour(0,1) fill(0)--> V
                                                  //| ector(4, 0), fill(1) em
                                                  //| Output exceeds cutoff limit.
  
  pouring.solution(6)                             //> res2: Stream[week7.l4_pouring_water.pouring.Path] = Stream(fill(1) pour(1,0
                                                  //| ) empty(0) pour(1,0) fill(1) pour(1,0)--> Vector(4, 6), ?)
  
  val pouring2 = new Pouring (Vector (4,9))       //> pouring2  : week7.l4_pouring_water.Pouring = week7.l4_pouring_water$Pouring
                                                  //| @6971e641
  pouring.solution(6)                             //> res3: Stream[week7.l4_pouring_water.pouring.Path] = Stream(fill(1) pour(1,0
                                                  //| ) empty(0) pour(1,0) fill(1) pour(1,0)--> Vector(4, 6), ?)
}