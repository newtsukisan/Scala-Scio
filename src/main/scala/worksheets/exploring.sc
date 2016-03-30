import competition.Explorer
import commons.allCombinations
import commons.time
import commons.timing




//val solt = explorer.getSolutionFor(11)


// real solution    [1,3,7,10,25,50] 765
// val explorerReal = new Explorer (Vector(1,3,7,10))
// time (explorer.getSolutionFor(280)



val numbers = Vector(1,3,7,10,25,50)
val explorer = new Explorer (numbers)

val com = allCombinations(List(0,2,3))
com.filter(explorer.valid_mult) map (lst => explorer.Suma(lst.head,lst.last))
val lista = List(1,2)


explorer.movesFrom1 (explorer.State(Vector(3,2,1),0))
time (explorer.getSolutionFor(765))
timing(explorer.getSolutionFor(765))

