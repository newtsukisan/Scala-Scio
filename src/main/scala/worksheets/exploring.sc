import competition.Explorer

val numbers = Vector(1,2,3)
val explorer = new Explorer (numbers)
explorer.initialOperands
val state: explorer.State = explorer.State(Vector(1,2),3)
state.update(1,2,3)

