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
  val initialState = State(initialOperands, 0)

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

  /**
    * From a state, calculate all possibles combinations of operands and all possible operations can be executed
    * @param state  initial state
    * @return       all possible operations of combining all operands in the state
    */
  def movesFrom(state: State) = {
    val operandos = state.operandos    // operandos are all numbers in a state
    val resultados = (for(op1 <- operandos; op2 <- operandos; if op1 > op2 ) yield
      Suma(op1, op2)) ++
      (for(op1 <- operandos; op2 <- operandos; if op1 > op2 ) yield
        Producto(op1, op2)) ++
      (for(op1 <- operandos; op2 <- operandos; if op1 > op2 ) yield
        Resta(op1, op2)) ++ //only integer divisions
      (for(op1 <- operandos; op2 <- operandos; if op1 > op2; if op1 % op2 == 0 ) yield
        Division(op1, op2))
    resultados
  }


}//end class