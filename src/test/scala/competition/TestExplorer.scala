package competition

import org.scalatest._

/**
  * Created by trabajo on 28/03/16.
  */
class TestExplorer extends FlatSpec{

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

  "A explorer" should "have a initial State"   in {
    assert(explorer.initialState === explorer.State(Vector(1,2,3),0))
  }
    it must "generate all possibles operations from one state" in {
      assert(explorer.movesFrom(explorer.initialState) === firstMove)
    }


}
