import competition.Explorer
import commons.allCombinations

val numbers = Vector(1,3,7,10,25,50)
val explorer = new Explorer (numbers)

val com = allCombinations(numbers.reverse.toList)
com.filter(explorer.valid_mult) map (lst => explorer.Suma(lst.head,lst.last))
val lista = List(1,2)

//Definimos una lista de filtros
//Podieamos tener una lista de tuplas
val operadores = List(explorer.Suma,explorer.Producto)
val filtros    = List(explorer.valid_sum _,explorer.valid_mult _)
println("Zip de operadores y filtros")
val parejas    =operadores.zip(filtros)

// para cada filtro aplicamos el filtro a las combinacines.
val filtrados = filtros map (f => com.filter(f))

//(operadores zip filtrados).map (case (clase: explorer.Suma,lista) => clase)


val por = List(1,2,3).zip(List('a','b','c'))
por map {case (x, y) => x }

println("creating values")

val total = (operadores zip filtrados).map {case (clase,lista) => lista.map {case List(op1,op2) => clase (op1,op2)}}
total.flatten










