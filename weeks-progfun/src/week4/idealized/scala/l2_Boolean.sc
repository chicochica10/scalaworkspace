package week4.idealized.scala

object Test {
  //vamos a ver que Boolean es una clase
  abstract class Boolean2 {

    //!! ojo se está hanciendo las llamadas por call-by-name strategy
    // implica que si el parametro no tiene que ser evaluado no lo será

    //https://class.coursera.org/progfun-003/forum/thread?thread_id=882

    //vamos a simular if (cond) then_expression else else_expression

    //cond.ifThenElse (then_expression, else_expresion)
    def ifThenElse[T](t: => T, e: => T): T

    // cond  de Boolean 2 && condicion X
    def &&(x: => Boolean2): Boolean2 = ifThenElse(x, false2)
    def ||(x: => Boolean2): Boolean2 = ifThenElse(true2, x)
    def unary_!(): Boolean2 					 = ifThenElse(false2,true2)
    
    def == (x: Boolean2): Boolean2 = ifThenElse(x, !x)
    def != (x: Boolean2): Boolean2 = ifThenElse(!x, x) //justo al reves
    def < (x: Boolean2): Boolean2 = ifThenElse (false2,x) // false < true

  }

  object true2 extends Boolean2 {
    def ifThenElse[T](t: => T, e: => T): T = t //e no se evalua al pasarlo por nombre
     //def &&(x: => Boolean2): Boolean2 = ifThenElse(x, ) // se evalua x
     //def ||(x: => Boolean2): Boolean2 = ifThenElse(true2, ) // se evalue true2
     //def unary_!(): Boolean2 					 = ifThenElse(false2, ) // se evalua false2
     //def == (x: Boolean2): Boolean2 = ifThenElse(x, ) // se evaluea x como estamos en true2 si x es true2 sera true2 y si x es false2 será false2
     //def != justo al reves
     //def < (x: Boolean2): Boolean2 = ifThenElse (false2 , )
  }

  object false2 extends Boolean2 {
    def ifThenElse[T](t: => T, e: => T): T = e //t no se evalua al pasarlo por nombre
     //def &&(x: => Boolean2): Boolean2 = ifThenElse( , false2) // se evalua false2
     //def ||(x: => Boolean2): Boolean2 = ifThenElse( , x) //se evalua x
     //def unary_!(): Boolean2 					 = ifThenElse( ,true2) // se evalua true2
     //def == (x: Boolean2): Boolean2 = ifThenElse (  ,!x) // se evalua !x si x=true2 => !x=false2
     //def != justo al reves																											// y si x=false2 => !x=true2
     //def < (x: Boolean2): Boolean2 = ifThenElse ( ,x)
  }

  //ejemplos con string
  true2.ifThenElse("yes", "no")                   //> res0: String = yes
  false2.ifThenElse("yes", "no")                  //> res1: String = no
  
  //ejemplos con Boolean2
  true2.ifThenElse (true2,false2)                 //> res2: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$true
                                                  //| 2$@90f25dc
  false2.ifThenElse (true2,false2)                //> res3: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fals
                                                  //| e2$@726343c4
  

  	true2 && true2                            //> res4: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$true
                                                  //| 2$@90f25dc
    true2 && false2                               //> res5: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fals
                                                  //| e2$@726343c4
    false2  && true2                              //> res6: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fals
                                                  //| e2$@726343c4
    false2 && false2                              //> res7: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fals
                                                  //| e2$@726343c4
                     
     true2 || true2                               //> res8: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$true
                                                  //| 2$@90f25dc
     true2 || false2                              //> res9: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$true
                                                  //| 2$@90f25dc
     false2 || true2                              //> res10: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
     false2 || false2                             //> res11: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
    !true2                                        //> res12: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
    !false2                                       //> res13: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
     true2 == true2                               //> res14: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
     false2 == false2                             //> res15: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
     true2 == false2                              //> res16: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
     
     false2 == true2                              //> res17: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
    
 		 true2 != true2                   //> res18: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
 		 true2 != false2                  //> res19: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
 		 
 		 false2 != true2                  //> res20: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
 		 false2 != false2                 //> res21: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
 		 
 			!true2                    //> res22: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
 			
 			!false2                   //> res23: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
                                                  
    true2 < true2                                 //> res24: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
    true2 < false2                                //> res25: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4
                                                  
    false2 < true2                                //> res26: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$tru
                                                  //| e2$@90f25dc
    false2 < false2                               //> res27: week4.idealized.scala.Test.Boolean2 = week4.idealized.scala.Test$fal
                                                  //| se2$@726343c4

}