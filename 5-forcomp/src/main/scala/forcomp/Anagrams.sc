package forcomp

import common._

object MisAnagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.MisAnagrams.Word] = List(Aarhus, Aaron, Ababa, ab
                                                  //| ack, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, ab
                                                  //| ased, abasement, abasements, abases, abash, abashed, abashes, abashing, abas
                                                  //| ing, abate, abated, abatement, abatements, abater, abates, abating, Abba, ab
                                                  //| be, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviat
                                                  //| es, abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdo
                                                  //| minal, abduct, abducted, abduction, abductions, abductor, abductors, abducts
                                                  //| , Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberrati
                                                  //| on, aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, a
                                                  //| bhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abid
                                                  //| ing, Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abjec
                                                  //| tions, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, abl
                                                  //| ated, ablates, ablating,
                                                  //| Output exceeds cutoff limit.

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences =
    (w.toList.groupBy(ch => ch.toLower) map { case (a, b) => (a, b.length) }).toList.sortWith(_._1 < _._1)
                                                  //> wordOccurrences: (w: forcomp.MisAnagrams.Word)forcomp.MisAnagrams.Occurrenc
                                                  //| es
  // otra solucion
  //w.toLowerCase.toList.groupBy(c => c).map({ case (char, string) => (char, string.length) }).toList.sorted

  wordOccurrences("caCarea")                      //> res0: forcomp.MisAnagrams.Occurrences = List((a,3), (c,2), (e,1), (r,1))

  /**
   * Converts a sentence into its character occurrence list.
   * type Sentence = List[Word]
   * type Occurrences = List[(Char, Int)]
   */

  def sentenceOccurrences(s: Sentence) = {

    wordOccurrences(s mkString)
  }                                               //> sentenceOccurrences: (s: forcomp.MisAnagrams.Sentence)forcomp.MisAnagrams.O
                                                  //| ccurrences
  val sentence = List("cacarea", "caca")          //> sentence  : List[String] = List(cacarea, caca)
  sentenceOccurrences(sentence)                   //> res1: forcomp.MisAnagrams.Occurrences = List((a,5), (c,4), (e,1), (r,1))

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   * type Sentence = List[Word]
   * type Occurrences = List[(Char, Int)]
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    //recorro el diccionario dictionary que es una List[Word]
    //calculo las occurrencias de cada word
    // de alguna manera meter las word que tienen la misma ocurrencia agrupadas en una lista bajo la clave Ocurrences
    // mirar el ejemplo de telephone para inspirarme -> wordsForNum

    dictionary groupBy wordOccurrences            //> dictionaryByOccurrences: => Map[forcomp.MisAnagrams.Occurrences,List[forcom
                                                  //| p.MisAnagrams.Word]]

  dictionaryByOccurrences                         //> res2: Map[forcomp.MisAnagrams.Occurrences,List[forcomp.MisAnagrams.Word]] =
                                                  //|  Map(List((e,1), (i,1), (l,1), (r,1), (t,2)) -> List(litter), List((a,1), (
                                                  //| d,1), (e,1), (g,2), (l,1), (r,1)) -> List(gargled), List((a,1), (e,1), (h,1
                                                  //| ), (i,1), (k,1), (n,1), (s,3)) -> List(shakiness), List((e,2), (g,1), (n,1)
                                                  //| ) -> List(gene), List((a,2), (n,1), (t,1), (y,1)) -> List(Tanya), List((a,1
                                                  //| ), (d,1), (e,2), (h,1), (m,1), (n,2), (o,1), (s,3)) -> List(handsomeness), 
                                                  //| List((a,2), (c,1), (e,2), (k,1), (l,1), (m,1), (p,1), (r,1), (t,1)) -> List
                                                  //| (marketplace), List((a,1), (i,1), (l,2), (s,1), (v,1)) -> List(villas), Lis
                                                  //| t((d,2), (e,1), (h,2), (n,1), (r,1), (t,1), (u,1)) -> List(hundredth), List
                                                  //| ((a,3), (b,1), (c,1), (h,1), (i,2), (l,1), (o,1), (p,2), (r,1), (t,1), (y,1
                                                  //| )) -> List(approachability), List((d,1), (e,2), (l,1), (s,1), (t,2)) -> Lis
                                                  //| t(settled), List((a,1), (g,1), (i,3), (l,1), (n,2), (t,1), (z,1)) -> List(L
                                                  //| atinizing), List((a,1),
                                                  //| Output exceeds cutoff limit.
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =

    dictionaryByOccurrences get wordOccurrences(word) match {
      case None => List()
      case Some(words) => words
    }                                             //> wordAnagrams: (word: forcomp.MisAnagrams.Word)List[forcomp.MisAnagrams.Word
                                                  //| ]
  wordAnagrams("Elvis")                           //> res3: List[forcomp.MisAnagrams.Word] = List(Elvis, evils, Levis, lives, vei
                                                  //| ls)
  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  val occurrences = wordOccurrences("aabc")       //> occurrences  : forcomp.MisAnagrams.Occurrences = List((a,2), (b,1), (c,1))
  occurrences                                     //> res4: forcomp.MisAnagrams.Occurrences = List((a,2), (b,1), (c,1))

  //de roque
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
      case List() => List(List())
      case (ch, count) :: tail => {
        val tailCombinations = combinations(tail) // supongo que se construir las combinaciones de la cola

        tailCombinations ++ // a las combinaciones de la cola le concateno todas esas posibles combinaciones de la cola
          // añadiendole a cada combinacion todas cabezas que son una lista de pares
          //(primera parte: caracter de la cabeza ch, segunda parte:
          //numero de ocurrencias tenga la el caracter de cabeza desde 1 hasta el numero de ocurrencias maximo)
          (for {
            o <- tailCombinations
            i <- 1 to count
          } yield (ch, i) :: o)
      }
    }
  }                                               //> combinations: (occurrences: forcomp.MisAnagrams.Occurrences)List[forcomp.Mi
                                                  //| sAnagrams.Occurrences]
  combinations(occurrences)                       //> res5: List[forcomp.MisAnagrams.Occurrences] = List(List(), List((c,1)), Lis
                                                  //| t((b,1)), List((b,1), (c,1)), List((a,1)), List((a,2)), List((a,1), (c,1)),
                                                  //|  List((a,2), (c,1)), List((a,1), (b,1)), List((a,2), (b,1)), List((a,1), (b
                                                  //| ,1), (c,1)), List((a,2), (b,1), (c,1)))
  // NO SIRVE
  def combinations1(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case (ch, count) :: xs =>
      (for {
        i <- 0 to count

      } yield List((ch, i))).toList ::: combinations1(xs)
  }                                               //> combinations1: (occurrences: forcomp.MisAnagrams.Occurrences)List[forcomp.M
                                                  //| isAnagrams.Occurrences]

  occurrences                                     //> res6: forcomp.MisAnagrams.Occurrences = List((a,2), (b,1), (c,1))
  combinations1(occurrences)                      //> res7: List[forcomp.MisAnagrams.Occurrences] = List(List((a,0)), List((a,1))
                                                  //| , List((a,2)), List((b,0)), List((b,1)), List((c,0)), List((c,1)), List())
  // NO SIRVE
  def expandeTupla(tupla: (Char, Int)) /*:List[Occurrences]*/ = tupla match {
    case (ch, n) => (for {
      i <- 0 to n
    } yield List((ch, i)).combinations(i).toList).toList // sin el tolist tengo un vector cuyos elemento son listas
  }                                               //> expandeTupla: (tupla: (Char, Int))List[List[List[(Char, Int)]]]
  expandeTupla('a', 2)                            //> res8: List[List[List[(Char, Int)]]] = List(List(List()), List(List((a,1))),
                                                  //|  List())
  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   * Notas del enunciado
   * ===================
   *
   * The precondition for the subtract method is that the occurrence list y is a subset of the occurrence list x – if the list y has
   * some character then the frequency of that character in x must be greater or equal than the frequency of that character in y.
   * When implementing subtract you can assume that y is a subset of x.
   *
   * Hint: you can use foldLeft, and -, apply and updated operations on Map.
   *
   * Now we can finally implement our sentenceAnagrams method for sequences.
   */

  // que es foldleft?
  val myList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)//> myList  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  //suma de los numeros
  // recorre los elementos de la lista en a y acumula la suma en b partiendo de 0 (b inicialmente es el 0)
  myList.foldRight(0)((b, a) => b + a)            //> res9: Int = 55
  //concatena partiendo de X
  myList.foldLeft("X")((b, a) => b + a)           //> res10: String = X12345678910

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    (y.toMap.foldLeft(x.toMap)((mapaxmod, tuplay) => { //empieza con el mapa de x, almacena los resultados en mapaxmod (siendo el primero el mapa de x (el grande)
    																									// y recorre las tuplas de y
      val nuevaFreq = mapaxmod(tuplay._1) - tuplay._2 //para cada tupla de y se almacenará en mapaxmod modifcado por esta funcion:
      if (nuevaFreq <= 0) mapaxmod - tuplay._1         //busco en mapaxmod el caracter (tuple._1) de la tupla de y y le resto la frecuencia de esa tupla de y
      else mapaxmod.updated(tuplay._1, nuevaFreq)			// si obtengo un numero negativo o cero se la quito al mapa acumulador con -
    })).toList.sorted																// si no actualizo el mapa con la nueva frecuencia
    																								//el mapamodificado lo vuelvo a convertir en lista y lo devulvo ordenado
  }                                               //> subtract: (x: forcomp.MisAnagrams.Occurrences, y: forcomp.MisAnagrams.Occur
                                                  //| rences)forcomp.MisAnagrams.Occurrences
  
  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  val y = List(('r', 1))                          //> y  : List[(Char, Int)] = List((r,1))

  subtract(x, y)                                  //> res11: forcomp.MisAnagrams.Occurrences = List((a,1), (d,1), (l,1))



 /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

//Hint: First of all, think about the recursive structure of the problem: what is the base case,
//and how should the result of a recursive invocation be integrated in each iteration?
//Also, using for-comprehensions helps in finding an elegant implementation for this method.

  def sentenceAnagrams(s: Sentence): List[Sentence] = { //sentence es una lista de words asi que tenemos una lista de listas de words (strings)
    def AnagramasPorOcurrenciaEnFrase(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(List()) // si la lista de ocurrencias esta vacia, devuelvo una lista de sentencias vacias
      else
        for {
          comb <- combinations(occurrences) // listado de todas la combinaciones posibles para esas ocurrencias
          // recorro las palabras del dicionario de ocurrencias (devuelvo la lista vacia si no esta) para cada combinacion
          word <- dictionaryByOccurrences.getOrElse(comb, List())
          // all the sentences that come from subtracting from the occurrences
          // the occurrence list of the word
          //con esas palabras monto una frase quitando la palabra de la lista de ocurrencias
          sentence <- AnagramasPorOcurrenciaEnFrase(subtract(occurrences, wordOccurrences(word)))
          if !comb.isEmpty //eliminamos las combinaciones vacias
       
        } yield word :: sentence
    }

    AnagramasPorOcurrenciaEnFrase (sentenceOccurrences(s))// saco todas las ocurrencias en la frase en una lista de ocurrencias
    
  }                                               //> sentenceAnagrams: (s: forcomp.MisAnagrams.Sentence)List[forcomp.MisAnagram
                                                  //| s.Sentence]
  sentenceAnagrams(List("I", "love", "you"))      //> res12: List[forcomp.MisAnagrams.Sentence] = List(List(you, Io, Lev), List(
                                                  //| you, Lev, Io), List(you, olive), List(Io, you, Lev), List(Io, Lev, you), L
                                                  //| ist(Lev, you, Io), List(Lev, Io, you), List(olive, you))
}