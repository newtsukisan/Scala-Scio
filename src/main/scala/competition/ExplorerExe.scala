package competition

/**
  * Created by trabajo on 29/03/16.
  */
object ExplorerExe extends App{

  // [1,3,7,10,25,50] 765
  // 3,7,5,9 190
  val expl: Explorer =  new Explorer(Vector(3,7,5,9))
  println ("Solving...")
  println (expl.getSolutionFor(190))

}
