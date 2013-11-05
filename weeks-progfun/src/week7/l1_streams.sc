package week7

object l1_streams {
  println("streams")                              //> streams

  // NOTAS DE SEMANA 6 l2_handling_nested_sequences

  // para todos los elementos desde 2 hasta n (no includido) su division no tiene que ser exacta
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
  // fin notas
 
  // Encontrar el segundo primero entre 1.000 y 10.000

  ((1000 to 10000) filter isPrime)                //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(1009, 1013, 1019, 
                                                  //| 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097
                                                  //| , 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 12
                                                  //| 01, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 
                                                  //| 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399
                                                  //| , 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 14
                                                  //| 83, 1487, 1489, 1493, 1499, 1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 
                                                  //| 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657
                                                  //| , 1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 17
                                                  //| 53, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867, 
                                                  //| 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973
                                                  //| , 1979, 1987, 1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 20
                                                  //| 63, 2069, 2081, 2083, 20
                                                  //| Output exceeds cutoff limit.
  ((1000 to 10000) filter isPrime)(1)             //> res1: Int = 1013


  // ^ tenemos funciones potentes sobre colecciones
  // si no lo tendriamos que hacer de manera recursiva

  def secondPrime(low: Int, high: Int) = nthPrime(low, high, 2)
                                                  //> secondPrime: (low: Int, high: Int)Int

  def nthPrime(from: Int, to: Int, nth: Int): Int = {
    if (from >= to) throw new Error("no prime")
    else if (!isPrime(from)) nthPrime(from + 1, to, nth) // descarto todos lo que no sean primos
    else if (nth == 1) from //estoy en un from que es primo como saber quee es el n-esimo... pq voy a ir descontando 1 de n en llamadas recursivas hasta que n sea 1
    else nthPrime(from + 1, to, nth - 1)
  }                                               //> nthPrime: (from: Int, to: Int, nth: Int)Int

  secondPrime(1000, 10000)                        //> res2: Int = 1013

  // el problema de la funcion filter es que contruye todos los numero primos cuando solo queremos el segundo
  // vamos a intentar no computar la cola de una secuencia hasta que realmente se necesite
  // que puede ser nunca
  // para ello usamos los STREAMS
  // Se definen como:
  // la constante Stream.empty y el constructor Stream.cons :
  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
                                                  //> xs  : Stream.Cons[Int] = Stream(1, ?)
  // o tambien
  Stream(1, 2, 3)                                 //> res3: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  //podemos convertir una coleccion en un stream con toStream
  (1 to 1000).toStream                            //> res4: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  // comparacion entre rangos de Streams y rangos de Lists
  def streamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi)) //> streamRange: (lo: Int, hi: Int)Stream[Int]

  def listRange(lo: Int, hi: Int): List[Int] =
    if (lo >= hi) Nil
    else lo :: listRange(lo + 1, hi)              //> listRange: (lo: Int, hi: Int)List[Int]

  streamRange(1, 1000)                            //> res5: Stream[Int] = Stream(1, ?)
  listRange(1, 1000)                              //> res6: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1
                                                  //| 6, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 
                                                  //| 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
                                                  //|  54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72
                                                  //| , 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 9
                                                  //| 1, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 
                                                  //| 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 
                                                  //| 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 
                                                  //| 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 
                                                  //| 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 
                                                  //| 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 
                                                  //| 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 
                                                  //| 198, 199, 200, 201, 202
                                                  //| Output exceeds cutoff limit.
  //encontrar el segundo primo con streams
  ((1000 to 10000).toStream filter isPrime)       //> res7: scala.collection.immutable.Stream[Int] = Stream(1009, ?)

  ((1000 to 10000).toStream filter isPrime)(1)    //> res8: Int = 1013
  //NOTA x::xs siempre da como resultado una lista nunca un stream
  // pero x #:: xs es igual que Stream.cons (x,xs)  y se puede usar en patrones

  // la principal diferencia entre streams y list es que en el constructor cons la cola se pasa por nombre

  // ejercicio
  def streamRange2(lo: Int, hi: Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange2(lo + 1, hi))
  }                                               //> streamRange2: (lo: Int, hi: Int)Stream[Int]
  streamRange2(1,10)                              //> 1 res9: Stream[Int] = Stream(1, ?)
  streamRange2(1,10).take(3)                      //> 1 res10: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  streamRange2(1,10).take(3).toList               //> 1 2 3 res11: List[Int] = List(1, 2, 3)
}