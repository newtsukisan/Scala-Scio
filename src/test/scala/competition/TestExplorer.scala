package competition

import org.scalatest._
import commons.timing

/**
  * Created by trabajo on 28/03/16.
  */
class TestExplorer extends FlatSpec{

  val time_require: Long  = 5000000000L

  val explorer = new Explorer(Vector(1,2,3))
  val state: explorer.State = explorer.State(Vector(1,2),3)

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


  "This explorer" should "have a initial State"   in {
    assert(explorer.initialState === explorer.State(Vector(1,2,3).sortWith(_ > _),0))
  }
  it must "have a correct valid_sum" in {
    assert(explorer.valid_sum(List(1,2)) === false)
    assert(explorer.valid_sum(List(2,1)) === true)
  }
  it must "have a correct valid_substract" in {
    assert(explorer.valid_substract(List(1,2)) === false)
    assert(explorer.valid_substract(List(2,1)) === true)
  }
  it must "have a correct valid_mult" in {
    assert(explorer.valid_mult(List(2,0)) === false)
    assert(explorer.valid_mult(List(0,3)) === false)
    assert(explorer.valid_mult(List(1,3)) === false)
    assert(explorer.valid_mult(List(3,1)) === false)
    assert(explorer.valid_mult(List(2,3)) === true)
    assert(explorer.valid_mult(List(3,2)) === true)
  }
  it must "have a correct valid_div" in {
    assert(explorer.valid_div(List(2,0)) === false)
    assert(explorer.valid_div(List(0,2)) === false)
    assert(explorer.valid_div(List(5,3)) === false)
    assert(explorer.valid_div(List(3,5)) === false)
    assert(explorer.valid_div(List(6,3)) === true)
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
  it must "have time constraints" in {
    val numbers = Vector(1,3,7,10,25,50)
    val explorer = new Explorer (numbers)
    assert(timing(explorer.getSolutionFor(765)) < time_require)
  }
}
