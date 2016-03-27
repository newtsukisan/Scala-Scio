package basic

/**
  * Created by trabajo on 27/03/16.
  */
import collection.mutable.Stack
import org.scalatest._

class TestRecurrencia extends FlatSpec {


  "A sorter"  must "maintain length of list" in {
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
    val test_list   = Recurrencia.sorter(lista_1)
    assert(Recurrencia.sorter(lista_1) === List(11,13,55,77,256))
  }
}
