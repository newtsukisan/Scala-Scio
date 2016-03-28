import basic.Recurrencia._


sorter(List(1,4,2))
val Juan  = Person("Juan",List())
val Sonia = Person("Sonia",List())
val Pedro = Person("Pedro",List(Sonia,Juan))
val Clara = Person("Clara",List(Pedro))

("Solo" :: List("esto","Hola")) mkString(", ")

(List(Juan, Juan) map (_.name)).mkString(" ,")
List(1,4,5,55).mkString(" ,")
//Podemos utilizar el currying para aplicar la funcion
// parcialemente
hasName("Pedro")(Pedro)
//Buscamos los amigos de clara que se llamen de determinada
//manera
showFromLop(getPersons(Clara,hasName("Clara")))

showFromLop(getPersons (Clara,hasName("Pedro"))  map (getFriends) flatten)

getPersons(Clara,hasName("Pedro"))

List().length

