package week7

object l3_work_with_infinite_secuences {
  println("work with infinite secuences")         //> work with infinite secuences

  // stream de todos los enteros empezando por uno
  def from(n: Int): Stream[Int] = n #:: from(n + 1)
                                                  //> from: (n: Int)Stream[Int]
  // todos lo numeros naturales
  val nats = from(0)                              //> nats  : Stream[Int] = Stream(0, ?)
  // todos los multiplos de 4
  val m4s = nats map (_ * 4)                      //> m4s  : scala.collection.immutable.Stream[Int] = Stream(0, ?)

  //los 100 primeros multiplos de 4
  (m4s take 100).toList                           //> res0: List[Int] = List(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 
                                                  //| 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104, 108, 112, 116, 120, 12
                                                  //| 4, 128, 132, 136, 140, 144, 148, 152, 156, 160, 164, 168, 172, 176, 180, 184
                                                  //| , 188, 192, 196, 200, 204, 208, 212, 216, 220, 224, 228, 232, 236, 240, 244,
                                                  //|  248, 252, 256, 260, 264, 268, 272, 276, 280, 284, 288, 292, 296, 300, 304, 
                                                  //| 308, 312, 316, 320, 324, 328, 332, 336, 340, 344, 348, 352, 356, 360, 364, 3
                                                  //| 68, 372, 376, 380, 384, 388, 392, 396)
  // cálculo de números primos por la criba de
  // eratóstenes

  //1.- empezar con todos los enteros desde 2 (el primer primo)
  //2.- eliminar los multiplos de 2
  //3.- la lista resultante empieza por 3 (numero primo)
  //4.- eliminar todos los multiplos de 3
  //5.- iterar ad infinitum, en cada paso el primer numero de la lista es primo y eliminamos sus multiplos

  def criba(s: Stream[Int]): Stream[Int] =
    // añade la cabeza a aquellos numeros cribados previamente tal que al dividirlos por la cabeza su resto no sea cero (no son multiplos)
    s.head #:: criba(s.tail filter (_ % s.head != 0))
                                                  //> criba: (s: Stream[Int])Stream[Int]
    //s.head #:: criba(s.tail filter (elem => elem % s.head != 0)) sin el _
    

  val cienPrimerosPrimos = criba(from(100)) take 20 toList
                                                  //> cienPrimerosPrimos  : List[Int] = List(100, 101, 102, 103, 104, 105, 106, 1
                                                  //| 07, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119)

  // vuelta al calculo de las raices cuadradas:

  // NOTAS PRIMERA SEMANA
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double
  def sqrt(x: Double) = {

    def sqrIter(aproximation: Double): Double =
      if (isGoodEnough(aproximation)) aproximation
      else sqrIter(improve(aproximation))

    def isGoodEnough(aproximation: Double) =
      abs(aproximation * aproximation - x) / x < 0.00001

    def improve(aproximation: Double) = (aproximation + (x / aproximation)) / 2

    sqrIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(64)                                        //> res1: Double = 8.000001655289593
  // FIN NOTAS

  // Con los streams podemos evitar la funcion is GoodeEnough para ver cuando terminan las iteraciones
  def sqrtStream(x: Double) = {
    def improve(aproximacion: Double) = (aproximacion + (x / aproximacion)) / 2

    //ojo debe de ser lazy!!! si no no compila
    lazy val aproximaciones: Stream[Double] = 1 #:: (aproximaciones map improve) //mapeo de aproximaciones a la funcion de improve
    // val aproximaciones: Stream[Double] = 1 #:: (aproximaciones map improve) //mapeo de aproximaciones a la funcion de improve
    aproximaciones
  }                                               //> sqrtStream: (x: Double)Stream[Double]

  sqrtStream(64) take (20) toList                 //> res2: List[Double] = List(1.0, 32.5, 17.234615384615385, 10.474036101145005
                                                  //| , 8.292191785986859, 8.005147977880979, 8.000001655289593, 8.00000000000017
                                                  //| , 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0, 8.0)
  // podemos añadir a posteriore la funcion isGoodEnough
  def isGoodEnough(aproximation: Double, x: Double) =
    abs(aproximation * aproximation - x) / x < 0.00001
                                                  //> isGoodEnough: (aproximation: Double, x: Double)Boolean
	// el _ es el numero calculado por sqrtStream
	sqrtStream(64) filter (isGoodEnough(_, 64))
                                                  //> res3: scala.collection.immutable.Stream[Double] = Stream(8.000001655289593,
                                                  //|  ?)
 //Consider two ways to express the infinite stream of multiples of a given number N:
 	val N = 10                                //> N  : Int = 10
  val xs = from(1) map (_ * N)                    //> xs  : scala.collection.immutable.Stream[Int] = Stream(10, ?)

  val ys = from(1) filter (_ % N == 0)            //> ys  : scala.collection.immutable.Stream[Int] = Stream(10, ?)
//Which of the two streams generates its results faster?
 xs take (100)                                    //> res4: scala.collection.immutable.Stream[Int] = Stream(10, ?)
 xs take (100) toList                             //> res5: List[Int] = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 1
                                                  //| 30, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 2
                                                  //| 80, 290, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390, 400, 410, 420, 4
                                                  //| 30, 440, 450, 460, 470, 480, 490, 500, 510, 520, 530, 540, 550, 560, 570, 5
                                                  //| 80, 590, 600, 610, 620, 630, 640, 650, 660, 670, 680, 690, 700, 710, 720, 7
                                                  //| 30, 740, 750, 760, 770, 780, 790, 800, 810, 820, 830, 840, 850, 860, 870, 8
                                                  //| 80, 890, 900, 910, 920, 930, 940, 950, 960, 970, 980, 990, 1000)
 
 ys take (100)                                    //> res6: scala.collection.immutable.Stream[Int] = Stream(10, ?)
 ys take (100) toList                             //> res7: List[Int] = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 1
                                                  //| 30, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 2
                                                  //| 80, 290, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390, 400, 410, 420, 4
                                                  //| 30, 440, 450, 460, 470, 480, 490, 500, 510, 520, 530, 540, 550, 560, 570, 5
                                                  //| 80, 590, 600, 610, 620, 630, 640, 650, 660, 670, 680, 690, 700, 710, 720, 7
                                                  //| 30, 740, 750, 760, 770, 780, 790, 800, 810, 820, 830, 840, 850, 860, 870, 8
                                                  //| 80, 890, 900, 910, 920, 930, 940, 950, 960, 970, 980, 990, 1000)
 //map es mas efectiente pruq no genera elementos incesarios
 // mientras que filter genera elementos que no van a cumplir
 // la condicion de tner resto cero y luego los desecha
 
 
}