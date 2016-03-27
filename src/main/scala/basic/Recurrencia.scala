package basic

/**
  * Created by trabajo on 27/03/16.
  */
object Recurrencia {

  /**
    * Simple sorter of list of integer using recurrence
    * @param lista
    * @return
    */
  def sorter  (lista:List[Int]) : List[Int] = lista match {
    case Nil          =>  Nil
    case (head::tail) => {
      def menores = tail filter (_ <= head)
      def mayores = tail filter (_ >  head)
      sorter (menores) ++ List(head) ++ sorter (mayores)
    }
  }

  /**
    * Simple case class for using recursion
    * @param name
    * @param friends
    */
  case class Person (name : String, friends : List[Person]) {
    override def toString: String = name
  }

  /**
    * Get list of person using natural recursion
    * @param person
    * @param name
    * @return
    */
  def getPersons (person: Person, name: String): List[Person] =  person match {
    case p if p.name == name => person ::  getPersonsfromFriends (person.friends, name)
    case _                   => getPersonsfromFriends (person.friends, name)
  }

  /**
    * ListofPerson, Name -> listofPerson
    * @param persons
    * @param name
    * @return
    */
  def getPersonsfromFriends(persons: List[Person],name: String): List[Person] = persons match{
    case Nil           => Nil           // Base Case
    case (head::tail)  => {             // Natural Recursion
      getPersons(head,name) ++ getPersonsfromFriends(tail, name)
    }
  }

  def hasName(p: Person, name: String): Boolean = name match {
    case p.name => true
    case _      => false


  }

}
