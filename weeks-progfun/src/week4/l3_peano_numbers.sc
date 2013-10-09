package week4

//utilizacion de los naturales sin usar typos primitivos
object l3_nat {
  println("PEANO NUMBERS")                        //> PEANO NUMBERS
// Peano numbers
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ (this)
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor =  throw new NoSuchElementException ("zero.predeccesor")
    //def successor = new Succ(this)// sirver new Succ (Zero), si pero con this puedo sacarla a la clase abstrac pq Succ tb la tiene
    def +(that: Nat) = that
    def -(that: Nat) = if (that isZero) this else throw new NoSuchElementException("zero.-")
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n // el numero del que quiero calcular su sucesor es su predecesor
    //def successor = new Succ(this)
    def +(that: Nat) = new Succ (n+that)//n + that es una unidad mas pequeño que el que queriemos pq n es el predecesor asi que hacemos el sucesor de es
    def -(that: Nat) =if (that.isZero) this else n - that.predecessor // estudiarlo un poco mas no lo acabo de entender
  }
}