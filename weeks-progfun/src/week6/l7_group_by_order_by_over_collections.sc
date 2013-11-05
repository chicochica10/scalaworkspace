package week6

object l7_group_by_order_by_over_collections {

  //ORDER BY
  val fruit = List("apple", "pear", "orange")     //> fruit  : List[String] = List(apple, pear, orange)
  fruit sortWith (_.length < _.length) //ordenar por tamaño
                                                  //> res0: List[String] = List(pear, apple, orange)
  //ordenar lexicograficamente
  fruit sorted                                    //> res1: List[String] = List(apple, orange, pear)

  //GROUP BY
  // parte una colecion en un map de collection mediante una funcion de discriminacion

  fruit groupBy (_.head)                          //> res2: scala.collection.immutable.Map[Char,List[String]] = Map(p -> List(pear
                                                  //| ), a -> List(apple), o -> List(orange))
  //polynomials
  //x^3 - 2x + 5
  Map(0 -> 5, 1 -> -2, 3 -> 1)                    //> res3: scala.collection.immutable.Map[Int,Int] = Map(0 -> 5, 1 -> -2, 3 -> 1)
                                                  //| 
  // vamos a representar en una clase los polinomios como mapas

  class Poly1(val terms: Map[Int, Double]) { // terms lleva val por lo que es un campo de class
    def +(other: Poly1) = new Poly1(terms ++ other.terms) //concatenacion tiene el problema de no hace la suma de los del mismo grado solo concatena
    override def toString =
      //los pares key value se pueden almacenar como tuplas
      //(for ((exp,coeff) <- terms) yield (coeff + "x^" + exp)) mkString " + " //si mkString no lleva nada se concatena con la cena vacia si no con lo que lleve
      //                                                    ^ ojo el mkString se hace con () sobre el for
      //como los da desordenados los ordennamos pero al reves
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }
  // mi solución
  class Poly2(val terms: Map[Int, Double]) {
    def +(other: Poly2) = new Poly2(terms ++ (other.terms map adjust)) //para que sume los coeficientes con igual exponente mapeamos other.terms para una funcion
    //                                                                 deje identicos los coeficientes de other.terms y sume los coeficientes de los exponentes que si existen en terms
    def adjust(term: (Int, Double)): (Int, Double) = term match {
      case (exp, coef) => terms get exp match {
        case None => (exp, coef)
        case Some(coef1) => (exp, coef + coef1)
      }
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  //la de odersky
  class Poly3(val terms: Map[Int, Double]) {
    def +(other: Poly3) = new Poly3(terms ++ (other.terms map adjust)) //para que sume los coeficientes con igual exponente mapeamos other.terms para una funcion
    //                                                                 deje identicos los coeficientes de other.terms y sume los coeficientes de los exponentes que si existen en terms
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coef) = term
      terms get exp match {
        case Some(coef1) => exp -> (coef + coef1)
        case None => exp -> coef
      }
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  // 6.2x^5 + 4x^3 + 2x
  val p1 = new Poly1(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p1  : week6.l7_group_by_order_by_over_collections.Poly1 = 6.2x^5 + 4.0x^3 +
                                                  //|  2.0x^1
  // 7x^3 + 3
  val p2 = new Poly1(Map(0 -> 3.0, 3 -> 7.0))     //> p2  : week6.l7_group_by_order_by_over_collections.Poly1 = 7.0x^3 + 3.0x^0

  val res = p1 + p2                               //> res  : week6.l7_group_by_order_by_over_collections.Poly1 = 6.2x^5 + 7.0x^3 
                                                  //| + 2.0x^1 + 3.0x^0
  //mi soulucion
  val p3 = new Poly2(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p3  : week6.l7_group_by_order_by_over_collections.Poly2 = 6.2x^5 + 4.0x^3 +
                                                  //|  2.0x^1
  val p4 = new Poly2(Map(0 -> 3.0, 3 -> 7.0))     //> p4  : week6.l7_group_by_order_by_over_collections.Poly2 = 7.0x^3 + 3.0x^0
  val res2 = p3 + p4                              //> res2  : week6.l7_group_by_order_by_over_collections.Poly2 = 6.2x^5 + 11.0x^
                                                  //| 3 + 2.0x^1 + 3.0x^0
  // la de odersky
  val p5 = new Poly3(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p5  : week6.l7_group_by_order_by_over_collections.Poly3 = 6.2x^5 + 4.0x^3 +
                                                  //|  2.0x^1
  val p6 = new Poly3(Map(0 -> 3.0, 3 -> 7.0))     //> p6  : week6.l7_group_by_order_by_over_collections.Poly3 = 7.0x^3 + 3.0x^0
  val res3 = p5 + p6                              //> res3  : week6.l7_group_by_order_by_over_collections.Poly3 = 6.2x^5 + 11.0x^
                                                  //| 3 + 2.0x^1 + 3.0x^0
  // forma mas sencilla de hacerlo
  // podemos definir mapas con valores por defecto de tal forma que si no se encuentra la clave devuelve el valor por defecto ej:
  val capitalOfCountry = Map("US" -> "Whasingthon", "Switzerland" -> "Bern")
                                                  //> capitalOfCountry  : scala.collection.immutable.Map[String,String] = Map(US 
                                                  //| -> Whasingthon, Switzerland -> Bern)
  val cap1 = capitalOfCountry withDefaultValue "<desconocida>"
                                                  //> cap1  : scala.collection.immutable.Map[String,String] = Map(US -> Whasingth
                                                  //| on, Switzerland -> Bern)
                
  cap1("andorra")                                 //> res4: String = <desconocida>

  class Poly4(terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly4) = new Poly4(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coef) = term
      exp -> (coef + terms(exp))
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p7 = new Poly4(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p7  : week6.l7_group_by_order_by_over_collections.Poly4 = 6.2x^5 + 4.0x^3 +
                                                  //|  2.0x^1

  val p8 = new Poly4(Map(0 -> 3.0, 3 -> 7.0))     //> p8  : week6.l7_group_by_order_by_over_collections.Poly4 = 7.0x^3 + 3.0x^0
  val res4 = p7 + p8                              //> res4  : week6.l7_group_by_order_by_over_collections.Poly4 = 6.2x^5 + 11.0x^
                                                  //| 3 + 2.0x^1 + 3.0x^0
  //demo del default value (no existe x^2)
  p7.terms(2)                                     //> res5: Double = 0.0


  // evitar la creacion de maps ha hacer los nuevos polinomios y pasar un numero variable de parametros
  class Poly5(terms0: Map[Int, Double]) {
  	def this (bindings: (Int, Double)*) = this (bindings.toMap) // constructor alternativo que toma un numero indeterminado de paramentros (*) y llama al contructor primario
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly5) = new Poly5(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coef) = term
      exp -> (coef + terms(exp))
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p9 = new Poly5(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)//> p9  : week6.l7_group_by_order_by_over_collections.Poly5 = 6.2x^5 + 4.0x^3 +
                                                  //|  2.0x^1
  val p10 = new Poly5(0 -> 3.0, 3 -> 7.0)         //> p10  : week6.l7_group_by_order_by_over_collections.Poly5 = 7.0x^3 + 3.0x^0
  val res5 = p9 + p10                             //> res5  : week6.l7_group_by_order_by_over_collections.Poly5 = 6.2x^5 + 11.0x^
                                                  //| 3 + 2.0x^1 + 3.0x^0
  
  // REESCRITURA USANDO: foldleft
  // que es foldleft?
  val myList = List (1,2,3,4,5,6,7,8,9,10)        //> myList  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  //suma de los numeros
  // recorre los elementos de la lista en a y acumula la suma en b partiendo de 0 (b inicialmente es el 0)
  myList.foldRight (0)((b,a) => b+a)              //> res6: Int = 55
  //concatena partiendo de X
  myList.foldLeft ("X")((b,a) => b + a)           //> res7: String = X12345678910
  
  
  class Poly6(terms0: Map[Int, Double]) {
  	def this (bindings: (Int, Double)*) = this (bindings.toMap) // constructor alternativo que toma un numero indeterminado de paramentros (*) y llama al contructor primario
    val terms = terms0 withDefaultValue 0.0
    def +(other: Poly6) = new Poly6(terms ++ (other.terms foldLeft terms)(addTerm)) //terms  es el "elemento 0" recorre other.terms y "acumula" los resultados con addTerm empezado por poner todos los terms
		def addTerm (terms: Map[Int, Double], term: (Int,Double)): Map [Int,Double] = { //terms empieza por el elemento 0 y recorre los elementos (term) de other.terms
			val (exp,coef) = term
			terms + (exp -> (coef + terms(exp))) //se añade directamente pq la version anterior necesitaba crear un map y luego concatenar
		}

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p11 = new Poly6(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
                                                  //> p11  : week6.l7_group_by_order_by_over_collections.Poly6 = 6.2x^5 + 4.0x^3 
                                                  //| + 2.0x^1
  val p12 = new Poly6(0 -> 3.0, 3 -> 7.0)         //> p12  : week6.l7_group_by_order_by_over_collections.Poly6 = 7.0x^3 + 3.0x^0
  val res6 = p11 + p12                            //> res6  : week6.l7_group_by_order_by_over_collections.Poly6 = 6.2x^5 + 11.0x^
                                                  //| 3 + 2.0x^1 + 3.0x^0
}