package commons

import org.scalatest._



/**
  * Created by trabajo on 28/03/16.
  */
class TestCommons extends FlatSpec{

  def numberOfCombinations (n: Int) : Int = n*(n-1)/2

     "combining"  must "combine simple list" in {
       assert(commons.combining(List(1,2,3),11) === List(List(11,1),List(11,2),List(11,3)))
     }
      it must "combine empty list gives empty list" in {
        assert(commons.combining(List(),1) === List())
      }
      "allCombinations" must "gives empty list on empty list" in {
        assert (commons.allCombinations(List()) === List())
      }
      it must " gives {{1,2}} for {1,2}" in {
        assert (commons.allCombinations(List(1,2)) === List(List(1,2)))
      }
      it must "gives {{1,2},{1,3},{2,3}} for {1,2,3}" in {
        assert (commons.allCombinations(List(1,2,3)) === List(List(1,2),List(1,3),List(2,3)))
      }
      it must "have right number of elements" in {
        assert (commons.allCombinations((1 to 10).toList).length === numberOfCombinations((1 to 10).length))
      }
}


