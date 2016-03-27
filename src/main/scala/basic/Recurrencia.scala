package basic

/**
  * Created by trabajo on 27/03/16.
  */
object Recurrencia {

  def sorter  (lista:List[Int]) : List[Int] = lista match {
    case Nil          =>  Nil
    case (head::tail) => {
      def menores = tail filter (_ <= head)
      def mayores = tail filter (_ >  head)
      sorter (menores) ++ List(head) ++ sorter (mayores)
    }

  }

}
