package week6

object l6_maps {
  println("maps")                                 //> maps
  
  val romanNumerals = Map ("I" -> 1, "V" -> 5, "X" -> 10)
                                                  //> romanNumerals  : scala.collection.immutable.Map[String,Int] = Map(I -> 1, V 
                                                  //| -> 5, X -> 10)
  val capitalOfCountry = Map ("US" -> "Whasingthon", "Switzerland" -> "Bern")
                                                  //> capitalOfCountry  : scala.collection.immutable.Map[String,String] = Map(US -
                                                  //| > Whasingthon, Switzerland -> Bern)
   // Los maps son Iterables pero tambien son funciones:
   capitalOfCountry ("US")                        //> res0: String = Whasingthon
   // si no existe da excepcion
   //capitalOfCountry ("Andorra")
   // para evitarlo:
   capitalOfCountry get "andorra"  //devuelve None//> res1: Option[String] = None
   capitalOfCountry get "US"                      //> res2: Option[String] = Some(Whasingthon)
   
   // que son las clases Option? ---> es un trait del que extienden
   // case class Some
   // object None
   // y se puede descomponer con pattern matching
   
   def showCapital (country: String) = capitalOfCountry get country  match {
   		case None => "Missing Data"
   		case Some (capital) => capital
   }                                              //> showCapital: (country: String)String
   
   showCapital ("US")                             //> res3: String = Whasingthon
   showCapital ("Andorra")                        //> res4: String = Missing Data
   // LOS MAPAS SOPORTAN LAS FUNCIONES SOBRE COLECCIONES Y SE PUEDEN HACER BUCLES FOR CON ELLOS
   
}