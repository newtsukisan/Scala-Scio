import basic.Recurrencia._

sorter(List(1,4,2))
val Juan  = Person("Juan",List())
val Sonia = Person("Sonia",List())
val Pedro = Person("Pedro",List(Sonia,Juan))
val Clara = Person("Clara",List(Pedro))


//Podemos utilizar el currying para aplicar la funcion
// parcialemente
hasName("Pedro")(Pedro)
//Buscamos los amigos de clara que se llamen Juan
getPersons(Clara,hasName("Pedro"))



