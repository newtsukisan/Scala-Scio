

package object commons {
  /** `???` can be used for marking methods that remain to be implemented.
    *  @throws   `Error`
    */
  def ??? : Nothing = throw new Error("an implementation is missing")

  type ??? = Nothing
  type *** = Any

  /**
    * Simple function for getting time of execution
    * @param block  Block of code to be execute
    * @tparam R     return type of block
    * @return       result of the block of code, or function
    */
  def time[R] (block: => R): R = {
    val t0 = System.nanoTime ()
    val result = block // call-by-name
    val t1 = System.nanoTime ()
    val t_final: Long =  t1 - t0
    println ("Elapsed time: " + t_final + " ns")
    result
  }

  def timing[R] (block: => R): Long = {
    val t0 = System.nanoTime ()
    val result = block // call-by-name
    val t1 = System.nanoTime ()
    val t_final: Long =  t1 - t0
    t_final
  }


  /**
    * Function for combining all elements of a list with another element
    * @param list   to combine with
    * @param e      element e
    * @tparam T     parametrize type
    * @return       list of list with pairs
    */
  def combining [T] (list: List[T],e: T): List[List[T]] = {
    for (elementsOflist <- list) yield List(e,elementsOflist)
  }

  /**
    * Create all possible combinations
    */
  def allCombinations [T] (lista: List[T]): List[List[T]] = lista match{
    case Nil                  => Nil                 // Border case
    case fst  :: scn :: Nil   => List(List(fst,scn)) // Base Case
    case head :: tail         => combining(tail,head) ++ allCombinations(tail) // Natural recursion
  }
}
