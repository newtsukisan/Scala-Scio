package competition

import org.scalatest._

/**
  * Created by trabajo on 28/03/16.
  */
class TestExplorer extends FlatSpec{

  val explorer = new Explorer(Vector(1,2,3))
  val state: explorer.State = explorer.State(Vector(1,2),3)

  "A State"  should "create a state"  in {
    assert(explorer.State (Vector(1,2,3),3)===explorer.State(Vector(1, 2, 3),3))
  }

  it should "update correctly" in {
    assert(state.update(1,2,3) === explorer.State(Vector(3),3))
  }

  "A explorer" should "have a initial State"   in {
    assert(explorer.intialState === explorer.State(Vector(1,2,3),0))
  }
}
