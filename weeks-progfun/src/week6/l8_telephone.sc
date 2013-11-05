package week6

import scala.io.Source

object l8_telephone {
  println("convertir números de teléfonos a frases")
                                                  //> convertir números de teléfonos a frases
  // da forbidden no funciona
  //val in = Source.fromURL ("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  val in = Source.fromURL("file:///home/angelrey/soft/scala/scalaworkspace/weeks-progfun/src/week6/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator

  // se transforma a lista para poder usarlo mas abajo para wordsForNum
  // val words = in.getLines
  // para la funcion wordsForNums eliminamos de la lista todos las palabras con caracteres raros
  // val words = in.getLines.toList
  // filtramos la lista por caracteres dentro de la palabra, todos los caracteres de la palabra deben ser letras
  val words = in.getLines.toList filter (word => (word forall (ch => ch.isLetter)))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.

  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnemonics  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -
                                                  //| > GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)

  //Invertir el mapa ej: A->2, B->2, C->2, D->3 ...
  // def charCode: Map[Char,Char]= for ((dig,txt) <- mnemonics; ch <-txt) yield ch -> dig
  //charCode

  //var que se pinte directamente poner val
  val charCode: Map[Char, Char] = for ((dig, txt) <- mnemonics; ch <- txt) yield ch -> dig
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J 
                                                  //| -> 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 
                                                  //| 5, B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, 
                                                  //| Z -> 9, S -> 7)
  //escribir para una palabra los numeros que la representan
  def wordCode(word: String): String =
    word.toUpperCase() map charCode               //> wordCode: (word: String)String

  wordCode("Java")                                //> res0: String = 5282

  // ahora al reves dado un número obtener una lista de todas
  // las palabras que puedo formar en base al dicionario de plabras words
  // y representarlo en un mapa p.e.: "5282"-> List ("Java", "Kata", "Lava",...)
  // Nota: odersky elige Seq porque es la clase padre de List

  // podemos hacer un agrupamiento como:
  val fruit = List("apple", "pear", "orange")     //> fruit  : List[String] = List(apple, pear, orange)
  fruit groupBy (_.head)                          //> res1: scala.collection.immutable.Map[Char,List[String]] = Map(p -> List(pea
                                                  //| r), a -> List(apple), o -> List(orange))

  val wordsForNum: Map[String, Seq[String]] =
    //words es un iterator que no hemos visto por ser imperativo asi que se tranforma words a una lista con tolist
    // no tiene en cuenta que en words hay caracteres raros como -

    // recuerda angel groupBy necesita el nombre de la funcion, no la ejecucion de la funcion ¡¡¡SIEMPRE LA PROGRAMACION FUNCIONAL!!!
    //words groupBy wordCode
    //como puede haber numeros que no tengan
    //asociada palabras ponemos la withDefault
    words groupBy wordCode withDefaultValue Seq() //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 29
                                                  //| 237638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> Li
                                                  //| st(allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86
                                                  //| 8437 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3
                                                  //| 364646489 -> List(femininity), 3987267346279 -> List(extraordinary), 785539
                                                  //| 7 -> List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 3
                                                  //| 86583 -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 
                                                  //| 847827 -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curl
                                                  //| icue), 84863372658 -> List(thunderbolt), 46767833 -> List(imported), 264374
                                                  //| 64 -> List(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(
                                                  //| spoolers), 46636233 -> List(homemade), 7446768759 -> List(rigorously), 7464
                                                  //| 4647 -> List(ringings), 633738 -> List(offset), 847825 -> List(visual), 772
                                                  //| 832 -> List(Pravda), 47
                                                  //| Output exceeds cutoff limit.

  // devuelve todas las formas de codificar  un numero como una lista de palabras
  // por lo hace por trozos: ejemplo para 7225247386: 7, 72, 722,7225 da un
  // conjunto de listas, cada lista son las palabras que se pueden formar con el 7, otra lista con las palabras del 72, etc...
  // cuando haya terminado con el 7 se desecha y se empieza con el 2, 22, 225...
  // luego se desecha y comienza de nuevo
  def encode(phoneNumber: String): Set[List[String]] =
    if (phoneNumber.isEmpty) Set(List())
    else {
      for {
        split <- 1 to phoneNumber.length() //pilla split y recorre todos los numeros cogiendo
        words <- wordsForNum(phoneNumber take split) //sin el get (pq le hemos añadido wordsForNum withDefault
        rest <- encode(phoneNumber drop split) // seguimos con el resto del numero de telefono
      } yield words :: rest
    }.toSet                                       //> encode: (phoneNumber: String)Set[List[String]]

  val phoneNumber = "7225247386"                  //> phoneNumber  : String = 7225247386
  encode(phoneNumber)                             //> res2: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, to
                                                  //| ), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), List(r
                                                  //| ack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bi
                                                  //| rd, to), List(Scala, is, fun), List(sack, bird, to))
  def translate (phoneNumber: String): Set[String] =
  	encode (phoneNumber) map (_ mkString " ") //> translate: (phoneNumber: String)Set[String]
  translate (phoneNumber)                         //> res3: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)
  
}