package competition

/**
  * * main class for exploring competition of guessing a number from combination from
  * a serie of numbers.
  * @param initialOperands   vector with the values for guessing the number.
  */
class Explorer (val initialOperands: Vector[Int]) {
  //--------------------------------------------------------------------------------------------------------------------
  /**
    * State is coded with a vector of operands and a result.
    * State has a update method which change the state by:
    *       taking off two operands of inner vector
    *       adding one result of operating both operands
    * @param operandos values to be used in obtaining next state
    * @param resultado State with two operands less and a new operand which is result of both operands
    */
  case class State(operandos: Vector[Int], resultado: Int ){
    /**
      * For updating state.
      * @param op1        first number which has been used
      * @param op2        second number which has been used
      * @param resultado  of operating both operands.
      * @return
      */
    def update(op1: Int, op2: Int, resultado: Int) = {  //update function
     val operandosNuevos = Vector(resultado) ++         //add new result
        operandos.filter(z => z != op1 && z != op2)     //drop operands used
       State(operandosNuevos, resultado)                //return new state
    }
  }// end case class State

  /*
    * Initial state. First result is zero.
    */
  //--------------------------------------------------------------------------------------------------------------------
  // sort from mayores to menores.
  val initialState = State(initialOperands.sortWith(_ > _), 0)
  //--------------------------------------------------------------------------------------------------------------------

  /*
   * trait operation. Has a method change which change state
   */
  trait Operation {
    def change(state: State): State
  }

  /**
    * Extends Operation. Add two operands
    * @param op1   First operand
    * @param op2   Second operand
    */
  case class Suma(op1: Int, op2: Int)  extends Operation{
    def change(state: State): State = state.update(op1, op2, op1 + op2)
  }
  /**
    * Extends Operation. Substract two operands
    * @param op1        First operand
    * @param restando   Second operand
    */
  case class Resta(op1: Int, restando: Int) extends Operation{
    def change(state: State): State = state.update(op1, restando, op1 - restando)
  }
  /**
    * Extends Operation. Multiply two operands
    * @param op1        First operand
    * @param op2        Second operand
    */
  case class Producto(op1: Int, op2: Int) extends Operation{
    def change(state: State): State = state.update(op1, op2, op1 * op2)
  }
  /**
    * Extends Operation. Add divide a cociente by divisor
    * @param cociente        First operand
    * @param divisor         Second operand
    */
  case class Division(cociente: Int, divisor: Int) extends Operation{
    def change(state: State): State = state.update(cociente, divisor, cociente / divisor)
  }


  //--------------------------------------------------------------------------------------------------------------------
  //-------------------------------- for this list we can filter for each operation ------------------------------------
  def valid_sum (par : List[Int]) : Boolean = par match {
    case List(op1,op2) if op1 > op2 => true
    case _                          => false
  }
  def valid_substract (par : List[Int]) : Boolean = par match {
    case List(op1,op2) if op1 > op2 => true
    case _                          => false
  }
  def valid_div (par : List[Int]) : Boolean = par match {
    case List (op1,op2) if  op1 == 0 || op2 == 0 => false
    case List (op1,op2) if  op2 == 1             => false
    case List (op1,op2) if  op1 % op2 != 0       => false
    case List (op1,op2) if  op1 < op2            => false
    case _                                       => true
  }
  def valid_mult (par : List[Int]) : Boolean = par match {
    case List (op1,op2) if  op1 == 1 || op2 == 1 => false
    case List (op1,op2) if  op1 == 0 || op2 == 0 => false
    case _                                       => true
  }
  //--------------------------------------------------------------------------------------------------------------------


  /**
    * next steps with all possible combinations of valid operators
    * @param state state from calculate all possibles operators.
    *
    */
  def movesFrom1 (state: State) = {
    val operandos       = state.operandos                             // fisrt obtaining operands
    val combinations    = commons.allCombinations (operandos.toList)  // all combinations
    val add_operands    = combinations                                //.filter(valid_sum)       is not need
    val sub_operands    = combinations                                //.filter(valid_substract) is not need
    val mul_operands    = combinations.filter(valid_mult)
    val div_operands    = combinations.filter(valid_div)
    val adds       =   add_operands map (lst => Suma(lst.head,lst.last))
    val subs       =   sub_operands map (lst => Resta(lst.head,lst.last))
    val muls       =   mul_operands map (lst => Producto(lst.head,lst.last))
    val divs       =   div_operands map (lst => Division(lst.head,lst.last))
    (adds ++ subs ++ muls ++ divs).toVector
  }//val adding_operands =

  //--------------------------------------------------------------------------------------------------------------------
  /**
    * This class is used for storing all states and all operations generated
    * @param stateHistory   list with all states generated
    * @param movesHistory   list with all movements generated
    */
  class Path(stateHistory: List[State], movesHistory: List[Operation]){
    def endState: State = if (stateHistory.isEmpty) initialState
        else stateHistory.head //last in first out
    /**
      * Add state and movements to history
      * @param state     new state to be added
      * @param move      new movemente to be added
      * @return          a path with this state and this movement added
      */
    def extend(state: State, move: Operation): Path = new Path(state :: stateHistory, move :: movesHistory)

    /**
      * for presenting path
      * @return
      */
    def representacion = {
      for (n <- movesHistory.indices)
        yield (movesHistory(n), stateHistory(n).operandos)
    }

    /**
      * String representation of the path
      *
      * @return
      */
    override def toString: String = initialState  + " -> " + (representacion.reverse mkString " -> ") + "-->" + endState.resultado
  }

  val initialPath = new Path(List(initialState), Nil)  // initial Path

  /**
    * From every singel path in the set of all paths,
    * get the last state.
    *
    * @param paths Set of all paths with every path of combination
    * @return      Set of all path with the new generated added
    */
  def from(paths: Set[Path]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path   <- paths
        state = path.endState
        // for every state, generate every possible movements (operation) and generate all possibles states
        next   <- movesFrom1(state) map (move => path.extend(move.change(state), move))
      }yield next //end for loop
      paths #:: from(more)  // adding the next generation
    }//end if else

  //--------------------------------------------------------------------------------------------------------------------
  val pathSets = from (Set(initialPath)) //All possible paths generated are store here
  //--------------------------------------------------------------------------------------------------------------------

  /**
    * looking for solution
    * @param target  number to be get
    * @return
    */
  private def solutionFor(target: Int): Stream[Path] =
    for{
      paths <- pathSets
      path  <- paths
      value = path.endState.resultado
      if value == target             // we have found solution
    } yield path

  /**
    *
    * @param target   the number which we are looking for
    * @return         the target with all states until solution
    */
  def getSolutionFor(target: Int): Stream[Path] = solutionFor(target)
  //Todo tolerance for no exact solutions


}//end class
