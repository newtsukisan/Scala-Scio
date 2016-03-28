package competition

import org.scalatest._

/**
  * Created by trabajo on 28/03/16.
  */
class TestExplorer extends FlatSpec{

  val explorer = new Explorer(Vector(1,2,3))
  val state: explorer.State = explorer.State(Vector(1,2),3)
  def validMu(op1: Int,op2: Int):Boolean  = explorer.validMult (op1,op2)   // for testing purposes
  def validDiv(op1: Int,op2: Int):Boolean = explorer.validDiv  (op1,op2)   // for testing purposes
  val firstMove =
    Vector(explorer.Suma(2,1), explorer.Suma(3,1), explorer.Suma(3,2), explorer.Producto(2,1),
      explorer.Producto(3,1), explorer.Producto(3,2), explorer.Resta(2,1), explorer.Resta(3,1),
      explorer.Resta(3,2), explorer.Division(2,1), explorer.Division(3,1))

  "A State"  should "create a state"  in {
    assert(explorer.State (Vector(1,2,3),3)===explorer.State(Vector(1, 2, 3),3))
  }
  it should "update correctly" in {
    assert(state.update(1,2,3) === explorer.State(Vector(3),3))
  }

  "validMul" must "gives false on 0 or 1" in {
    assert(validMu(0,2) === false)
    assert(validMu(2,0) === false)
    assert(validMu(2,1) === false)
    assert(validMu(1,2) === false)
    assert(validMu(2,3) === false)
    assert(validMu(3,2) === true)
  }
  "validDiv" must "gives false on zero values operands" in {
    assert(validDiv(0,2) === false)
    assert(validDiv(2,0) === false)
  }
  it         must "gives false on non integer division" in {
    assert(validDiv(5,2) === false)
  }
  it         must "gives false on result < 1 division" in {
    assert(validDiv(2,4) === false)
  }
  it         must "gives true on integer divisions op1 > op2" in {
    assert(validDiv(4,2) === true)
  }
  "This explorer" should "have a initial State"   in {
    assert(explorer.initialState === explorer.State(Vector(1,2,3),0))
  }

  it must "find a solution for 6" in {
    assert(explorer.getSolutionFor(6) != Stream())
  }
  it must "find a solution for 5" in {
    assert(explorer.getSolutionFor(5) != Stream())
  }
  it must "find a solution for 4" in {
    assert(explorer.getSolutionFor(4) != Stream())
  }
  it must "find a solution for 9" in {
    assert(explorer.getSolutionFor(9) != Stream())
  }
  it must "find a solution for 11" in {
    assert(explorer.getSolutionFor(11) != Stream())
  }
}
