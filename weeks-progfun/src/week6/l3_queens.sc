package week6

object l3_queens {
  //ejemplos con sets
  val fruit = Set("apple", "banana", "pear")      //> fruit  : scala.collection.immutable.Set[String] = Set(apple, banana, pear)
  val s = (1 to 6).toSet                          //> s  : scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3, 4)

  //operaciones con sets
  s map (_ + 2)                                   //> res0: scala.collection.immutable.Set[Int] = Set(5, 6, 7, 3, 8, 4)
  //fruit filter( _.startsWith ==  "app")
  fruit filter (s => s.startsWith("app"))         //> res1: scala.collection.immutable.Set[String] = Set(apple)
  s.nonEmpty                                      //> res2: Boolean = true
  //1.- Los conjuntos no estan ordenados
  //2.- no tienen elementos repes
  //3,- la principal funcion es contains

  //PROBLEMA DE DE LAS N-REINAS
  // colocar una reina en cada fila (se guarda la posicion (de 0 a n-1) dentro de una lista)
  // como puede haber varias soluciones hay varias listas que se guardan en un conjunto de listas
  // suponer que se han puesto k-1 reinas poner una reina en la siguiente fila donde no este amenazada por otra reina
  // y resolverlo para cada uno de los elementos (listas) del conjunto de soluciones

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =  {// creo una funcion pq necesito guardar el valor de n
      if (k == 0) Set(List())
      else for {
          queens <- placeQueens(k - 1) //queens es una lista donde estan las reinas anteriores (sirver como varible al recorrido de las listas anteriores) que no este en "check" con ninguna diagonal
          col <- 0 until n // col : distintas posiciones de columnas donde poner ala reina
          if isSafe(col, queens)
        } yield col :: queens //devuelve la lista de de posiciones antereiores donde las reinas no se amezan con la nueva posicion donde la nueva reinan no es amenazada
    }
    
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  // asumismos que la reina se pone en la siguiente fila es decir en queen.lenght
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length//tenemos ya col y row de la posicion de la nueva reina
    //sacamos los pares de (row,col) de las reinas anteriores
    val queensWithRow = (row - 1 to 0 by -1) zip queens// asi tengo los pares (2,3)(1,0)(02)
    // queensWithRow forall (((r: Int, c: Int) => true)) no le gusta al compilador hacerlo con case
    //  esta linea de abajo se puede sustituir por la siguiente pq listElem no se emplea
    //  queensWithRow forall  (listElem => listElem match{
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r//col tiene que estar en una columana distinta del resto,
      																											 //en cuanto a la linea nada pq es una nueva linea y lo de math... es para las diagonales
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  queens(4)                                       //> res3: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
// ahora ponemos visualizacion

	def show (queens: List[Int]) = {
		
		val lines = for (col <- queens.reverse) // voy a pintar lineas, la ultima columna metida es la primera
		//yield col
		//yield Vector.fill(queens.length)("* ") //lista de vectores, la lista tiene de tamaño col, los vectores tienen de tamaño queens.length y estan rellenos con *
		//yield Vector.fill(queens.length)("* ").updated (col,"X ")// actualizo la posicion col con una x en cada vector
		yield Vector.fill(queens.length)("* ").updated (col,"X ").mkString + "\n" //transforma los vectores en strings
		
		//"\n" + lines me lo saca como una lista de strings
		"\n" + lines.mkString
	
	
	}                                         //> show: (queens: List[Int])String
	
	show (List(3,2,1,0))                      //> res4: String = "
                                                  //| X * * * 
                                                  //| * X * * 
                                                  //| * * X * 
                                                  //| * * * X 
                                                  //| "
	
	//queens (4) map show //devuleve un set al que le a aplicado show
    (queens (4) map show) mkString                //> res5: String = "
                                                  //| * * X * 
                                                  //| X * * * 
                                                  //| * * * X 
                                                  //| * X * * 
                                                  //| 
                                                  //| * X * * 
                                                  //| * * * X 
                                                  //| X * * * 
                                                  //| * * X * 
                                                  //| "
}