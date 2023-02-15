package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match{
    
    case Empty => this
    case Cons(h,t) => Cons(h,prefix(t)) 
  }
  override def size: Int = this match{
    
    case Empty => 0
    case _ => 1+tail.size
  }

  override def map(mapFunc: Int => Int): IntList = this match{
    
    case Empty => Empty
    case Cons(_,_) => Cons(mapFunc(head), tail.map(mapFunc))
        
  }

  override def filter(filterFunc: Int => Boolean): IntList =  this match{
    
    case Empty => Empty
    case Cons(_,_) => if (filterFunc(head)) Cons(head, tail.filter(filterFunc)) else tail.filter(filterFunc)
  }

  override def forAll(predicateFunc: Int => Boolean): Boolean = this match{

    // Es reicht wenn ein Element false liefert um die Funktion zu beenden, beim letzten
    // Element ist die Schleife durch, wenn wir bis dahin gekommen sind, sind bis dahin alle true
    case Empty => false
    case Cons(_,Empty) => if (predicateFunc(head)) true else false
    case Cons(_,_) => if (predicateFunc(head))  tail.forAll(predicateFunc) else false


  }

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {

    case Empty => reduceFunc (initial, 0)
    case Cons(_, Empty) => reduceFunc(initial, head)
    case Cons(_, _) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this match {

    case Empty => 0
    case Cons(_,Empty) => head
    case Cons(head,tail) => Cons(reduceFunc(head, tail.head), tail.tail).reduceLeft(reduceFunc)
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {

    case Empty => initial
    case Cons(head, tail) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match{

    case Cons(_, Empty) => head
    case Cons(head, tail) => reduceFunc(head, tail.reduceRight(reduceFunc))


  }
  override def insertSorted(elem: Int): IntList = this match {

    case Cons(head, Empty) => if (head>elem) this.prefix(Cons(elem, Empty)) else Cons(head, Cons(elem, Empty))


    case Empty => Cons(elem, Empty)
    case Cons(head, tail) if (head > elem) => Cons(elem, Cons(head,tail))
    case Cons(head, tail) if (tail.head > elem) => Cons(head, Cons(elem, tail))
    case Cons(head, tail) => Cons(head, tail.insertSorted(elem))


  }

  override def insertionSort: IntList = {

       def isort(li: IntList): IntList = {
         if (li.isEmpty) Empty
         else insert(li.head, isort(li.tail))
       }

    def insert(x: Int, li:IntList): SinglyLinkedIntList = {

      if ( li.isEmpty || x <= li.head) Cons(x, li)
      else Cons(li.head, insert(x, li.tail))
    }
    isort(this)
  }
}