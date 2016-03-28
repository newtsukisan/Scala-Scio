package basic

/**
  * Created by trabajo on 27/03/16.
  */
import basic.Recurrencia._


import org.scalatest._

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

class TestRecurrencia extends FlatSpec {
  /**
    * Simple Testing values
    */
  val Juan  = Person("Juan",List())
  val Sonia = Person("Sonia",List())
  val Pedro = Person("Pedro",List(Sonia,Juan))
  val Clara = Person("Clara",List(Pedro))

  "Juan" should "have himself as a  friend" in {
    assert(getPersons(Juan, hasName("Juan")).nonEmpty)
  }
  it should "has not a friends with name Clara" in {
    assert(getPersons(Juan,hasName("Pedro")).length === 0)
  }
  it should "has not friends" in {
    assert(Juan.friends === List())
  }
  it should "has a name which is Juan" in {
    assertResult("Juan"){
      Juan.name
    }
  }

  "Clara" must "have a friend whose name is Juan" in {
    assert(getPersons(Clara,hasName("Juan")) === List(Juan))
  }
   it must "have a friend whose name is Pedro and Pedro has Juan and Sonia as friends"  in {
     val amigo_Clara  = getPersons(Clara,hasName("Pedro"))
     val amigos_Amigo =showFromLop(amigo_Clara.flatMap(getFriends))
     assert(amigo_Clara  === List(Pedro))
     assert(amigos_Amigo === "Pedro, Sonia, Juan")
   }

  "A sorter"  must "maintain length of list" taggedAs DbTest in {
    val lista_1     = List(1,2,3,5,7)
    val long_1      = lista_1.length
    val test_long   = Recurrencia.sorter(lista_1).length
    assert(long_1 === test_long)
  }
  it must "order well empty list and list with one element"  in {
    val lista_1   = List(3)
    val lista_2   = Nil
    val test_list_1 =  Recurrencia.sorter(lista_1)
    val test_list_2 =  Recurrencia.sorter(lista_2)
    assert(lista_1 === test_list_1)
    assert(lista_2 === test_list_2)
  }
  it must "order well a list" in {
    val lista_1     = List(11,256,13,55,77)
    assert(Recurrencia.sorter(lista_1) === List(11,13,55,77,256))
  }
}
