package week7

object l6_pouring_water_optimizacion2 {

  type GlassCapacity = Int
  type Glass = Int
  type States = Vector[GlassCapacity]
  type Capacities = Vector[GlassCapacity]

  class Pouring(capacities: Capacities) {

    val initialStates: States = capacities map (x => 0)

    trait Move {
      def changeState(state: States): States
    }
    case class fill(glass: Glass) extends Move {
      def changeState(states: States) = states updated (glass, capacities(glass))
    }
    case class empty(glass: Glass) extends Move {
      def changeState(states: States) = states updated (glass, 0)
    }

    case class pour(from: Glass, to: Glass) extends Move {
      def changeState(states: States) = {
        val amount = states(from) min (capacities(to) - states(to))
        states updated (from, states(from) - amount) updated (to, states(to) + amount)
      }
    }

    val glasses = 0 until capacities.length

    val fillGlasses = for (glassToFill <- glasses) yield fill(glassToFill)
    val emptyGlasses = for (glassToEmpty <- glasses) yield empty(glassToEmpty)
    val pourGlasses = for (fromGlass <- glasses; toGlass <- glasses if (fromGlass != toGlass)) yield pour(fromGlass, toGlass)

    val moves = fillGlasses ++ emptyGlasses ++ pourGlasses


    // viene de abajo, pasamos endStates1 como parametro
    //añado val para que la propiedad sea visible desde el exterior
    class Path(history: List[Move],val endStates1: States) {
      //otra forma de hacerlo mas elegante: con foldRight (no es optimizacion, ya estaba en el anterior)
      //def endStates1: States = (history foldRight (initialStates))(_ changeState _)
      //cuando añadimos un movimiento a la historia tb consideramos los cambios en endStates
      def addMove(move: Move) = new Path(move :: history, move changeState endStates1)
      override def toString = (history.reverse mkString " ") + "--> " + endStates1
    }

    val initPath = new Path(Nil, initialStates)

		//endStates1 se llama 2 veces y otra mas en la solucion... podemos mejorarlo (ya que es un foldright que es recursivo)
		//vamos a pasarlo como parametro en la clase path ^ (*)
    def from(paths: Set[Path], exploredStates: Set[States]): Stream[Set[Path]] = {
      if (paths.isEmpty) Stream.Empty
      else {
        val newPaths = for {
          path <- paths
          newPath <- moves map path.addMove
          if (!(exploredStates contains newPath.endStates1))
        } yield newPath
        paths #:: from(newPaths, exploredStates ++ (newPaths map (_.endStates1)))
      }
    }

    val pathsSet = from(Set(initPath), Set(initialStates))

    def solution(target: Int): Stream[Path] = for {
      pathSet <- pathsSet
      path <- pathSet
      if (path.endStates1 contains target)
    } yield path
  }

  val capacities = Vector(4, 9)                   //> capacities  : scala.collection.immutable.Vector[Int] = Vector(4, 9)
  val pouring = new Pouring(capacities)           //> pouring  : week7.l6_pouring_water_optimizacion2.Pouring = week7.l6_pouring_
                                                  //| water_optimizacion2$Pouring@22c0468e
  pouring.pathsSet.take(3).toList                 //> res0: List[Set[week7.l6_pouring_water_optimizacion2.pouring.Path]] = List(S
                                                  //| et(--> Vector(0, 0)), Set(fill(0)--> Vector(4, 0), fill(1)--> Vector(0, 9))
                                                  //| , Set(fill(0) fill(1)--> Vector(4, 9), fill(0) pour(0,1)--> Vector(0, 4), f
                                                  //| ill(1) fill(0)--> Vector(4, 9), fill(1) pour(1,0)--> Vector(4, 5)))

  pouring.solution(6)                             //> res1: Stream[week7.l6_pouring_water_optimizacion2.pouring.Path] = Stream(fi
                                                  //| ll(1) pour(1,0) empty(0) pour(1,0) empty(0) pour(1,0) fill(1) pour(1,0)--> 
                                                  //| Vector(4, 6), ?)

}