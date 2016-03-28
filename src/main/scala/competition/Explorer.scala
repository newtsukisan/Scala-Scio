package competition

/**
  * * main class for exploring competition of guessing a number from combination from
  * a serie of numbers.
  * @param initialOperands   vector with the values for guessing the number.
  */
class Explorer (val initialOperands: Vector[Int]) {

  /**
    * State is coded with a vector of operands and a result.
    * State has a update method which change the state by:
    *       taking off two operands of inner vector
    *       adding one result of operating both operands
    * @param operandos
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
  val intialState: State = State(initialOperands, 0)

}//end class
