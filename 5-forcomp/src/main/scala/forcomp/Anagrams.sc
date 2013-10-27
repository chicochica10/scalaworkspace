package forcomp

import common._

object MisAnagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
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

  /** The dictionary is simply a sequence of words.
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
  

  /** Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences  =
   (w.toList.groupBy(ch => ch.toLower) map { case (a,b) => (a, b.length)}).toList.sortWith (_._1 < _._1)
                                                  //> wordOccurrences: (w: forcomp.MisAnagrams.Word)forcomp.MisAnagrams.Occurrenc
                                                  //| es
   // otra solucion
   //w.toLowerCase.toList.groupBy(c => c).map({ case (char, string) => (char, string.length) }).toList.sorted
  
  wordOccurrences ("caCarea")                     //> res0: forcomp.MisAnagrams.Occurrences = List((a,3), (c,2), (e,1), (r,1))

  /** Converts a sentence into its character occurrence list.
    type Sentence = List[Word]
  	type Occurrences = List[(Char, Int)]
   */
  
  def sentenceOccurrences(s: Sentence)  = {
  	wordOccurrences (s mkString)
  }                                               //> sentenceOccurrences: (s: forcomp.MisAnagrams.Sentence)forcomp.MisAnagrams.O
                                                  //| ccurrences
 	val sentence = List ("cacarea", "caca")   //> sentence  : List[String] = List(cacarea, caca)
	sentenceOccurrences (sentence)            //> res1: forcomp.MisAnagrams.Occurrences = List((a,5), (c,4), (e,1), (r,1))
}