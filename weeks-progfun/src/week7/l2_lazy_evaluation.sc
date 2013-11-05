package week7

object l2_lazy_evaluation {
  println("lazy evaluation")                      //> lazy evaluation
  //lazy dos cosas: 1. no hacer nada hasta que sea preciso y
  //								2. nunca hacerlo dos veces

  //el problema de los streams es que si se llama a la cola varias veces
  // el stream se recomputa cada vez

  //se puede solventar guardandos el resultado de la primera computacion de la cola
  // y reusandola en vez de recomputarla cada vez => esta evaluacion se llama lazy

  //no se usa por defecto la lazy evaluation pq es bastante impredecible cuando va a entrar en juego la
  // evaluacion, eso no importa en un lenguaje funcional
  // pero como scala no solo es funcional (permite tb estados mutables)
  // pueden darse situaciones "raras"

  // la definicion es asi
  lazy val x = ???                                //> x: => Nothing
  // la diferencia parametro con un paso por nombre es que aunque el de paso
  // por nombre tampoco se evalua hasta que se necesita, luego se recomputa
  // cada vez que se pasa por él mientra que con lazy val no

  // ejercicio:

  def expr = {
    val x = { print("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    z + y + x + z + y + x
  //3 + 2 + 1 + 3 + 2 + 1 = 12
  }                                               //> expr: => Int
  expr                                            //> xzyzres0: Int = 12
  
  // pereeeero aunque da 12
  // al llamar a exp... primero se evalua x y se imprime
  // luego y es delayed y la z tb (se define pero no se ejecuta
  // luego se computa z + y + x + z + y + x
  // al computartse z + y + x + z + y + x se ejecuta la deficion de z (se imprime z)
  // y luego se fuerza la evaluacion de y (se imprime y)
  // como x ya está evaluado no se imprime nada
  // despues de vuelve a evaluzar z (se imprime)
  // y ya está evaluaada y no vuelve a computar
  // al final se trata x, pero ya esta evaluada y no se imprime resultado fina xzyz
  
  
  // PARA HACER MAS EFICIENTES AUN LOS Streams no sólo se pasan por nombre la cola al cons si no que trabajamos internamente
  // no con la cola en si misma i no con un lazy val tail = tl (tl es el parámetro) ver diapositivas lecture 7.3
  // asi nos evitamos recomputaciones de tl
  
  //nota:
   def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
 

   ((1000 to 10000).toStream filter isPrime)(1)   //> res1: Int = 1013
   // es lo mismo que:
   ((1000 to 10000).toStream filter isPrime) apply 1
                                                  //> res2: Int = 1013
 
}