package week6

object l4_criteria_with_for {
  println("criteria with for")                    //> criteria with for
  //ojo tiene un case si ponemos el case no hace falta poner news pero si no tenemos que poner newS en los books de la lista
  // ver week4 l9_pattern_matching_1.sc
  case class Book(title: String, authors: List[String])

  // el poner title =, authors= parece optativo se llama 'name parameters' y a veces es más claro
  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs", authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming", authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java", authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers", authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "test book", authors = List("Bloch, Joshua")),
    Book(title = "Programming in Scala", authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : List[week6.l4_criteria_with_for.Book] = List(Book(Structure and Int
                                                  //| erpretation of Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)),
                                                  //|  Book(Introduction to Functional Programming,List(Bird, Richard, Wadler, Phi
                                                  //| l)), Book(Effective Java,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch
                                                  //| , Joshua, Gafter, Neal)), Book(test book,List(Bloch, Joshua)), Book(Programm
                                                  //| ing in Scala,List(Odersky, Martin, Spoon, Lex, Venners, Bill)))

  //libros cuyos autores empiezan por bird
  for {
    b <- books
    a <- b.authors
    if (a startsWith "Bird")
  } yield b                                       //> res0: List[week6.l4_criteria_with_for.Book] = List(Book(Introduction to Fun
                                                  //| ctional Programming,List(Bird, Richard, Wadler, Phil)))
  //libros que tengan la palabra "program en el título

  for {
    b <- books
    if (b.title indexOf ("Program")) > 0
  } yield b                                       //> res1: List[week6.l4_criteria_with_for.Book] = List(Book(Structure and Inter
                                                  //| pretation of Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)), 
                                                  //| Book(Introduction to Functional Programming,List(Bird, Richard, Wadler, Phi
                                                  //| l)))

  // encontrar los nombres de todos los autores que hayan escrito al menos dos libros
  for {
    b1 <- books
    b2 <- books
    if (b1 != b2) // recorre las dos series a la vez y lo que vaya a hacer al recorrer la serie tiene que ser cuando b1 sea distinto de b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res2: List[String] = List(Bloch, Joshua, Bloch, Joshua, Bloch, Joshua, Bloc
                                                  //| h, Joshua, Bloch, Joshua, Bloch, Joshua)

  //^ sale duplicado pq tenemos dos iteradores a la vez funcionando para evitarlo:

  for {
    b1 <- books
    b2 <- books
    if (b1.title < b2.title) // establecemos un orden lexicografico por el titulo del libro
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res3: List[String] = List(Bloch, Joshua, Bloch, Joshua, Bloch, Joshua)

  // pero si ha escrito 3 libros (he introducido uno de test) en la bbdd sale 3 veces ^
  // para evitarlo hay que quitar duplicados
 
 ( for {
    b1 <- books
    b2 <- books
    if (b1 != b2) // o  if (b1.title < b2.title)
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1 ).distinct                           //> res4: List[String] = List(Bloch, Joshua)
  //yield a1.distinct no sirve hay que poner parentesis al for tambien se puede poner {}

}