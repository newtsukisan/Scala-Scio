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
  case class Person (name : String, friends : List[Person])

  /**
    * Get list of person using natural recursion. Introduce an predicative function to select persons.
    * This function takes a person and all his friends and return only those that satisfied predicative
    * function
    * @param person
    * @param predicative
    * @return
    */
  def getPersons (person: Person, predicative: (Person => Boolean)): List[Person] =  person match {
    case p if predicative(p) => person ::  getPersonsfromFriends (person.friends, predicative)
    case _                   => getPersonsfromFriends (person.friends, predicative)
  }

  /**
    * ListofPerson, Name -> listofPerson
    * @param persons
    * @param predicative
    * @return
    */
  def getPersonsfromFriends(persons: List[Person],predicative: (Person => Boolean)): List[Person] = persons match{
    case Nil           => Nil           // Base Case
    case (head::tail)  => {             // Natural Recursion
      getPersons(head,predicative) ++ getPersonsfromFriends(tail, predicative)
    }
  }

  /**
    * This function
    * @param name
    * @param p
    * @return boolean if the person has this name
    */
  def hasName(name: String)(p: Person): Boolean = name match {
    case p.name => true
    case _      => false
  }

  def showFromLop (persons: List[Person]): String = (persons map (_.name)).mkString(", ")

  /**
    * Get a list of friends of a person. Take a person a all his friends
    * @param person
    * @return
    */
  def getFriends (person: Person) : List[Person] = {
    person :: getFriendsOfLOP (person.friends)
  }
  def getFriendsOfLOP(persons: List[Person]) : List[Person] = persons match{
    case Nil           => Nil
    case (head::tail ) => getFriends (head) ++ getFriendsOfLOP(tail)
  } //end method
  //--------------------------------------------------------------------------------------------------------------------
  /**
    * Using parameters for general use
    * @param lista    which elements are needs to be combined using variations
    * @param takenby  number of elements in each variation
    * @tparam T
    * @return         Set[List[T]]
    */
  def variations [T] (lista : List[T], takenby : Int) : Set[List[T]] = {
    val n = lista.length              // length of the list
    def inner (k : Int): Set[List[T]] = {
      if (k == (n - takenby)) Set(List())
      else
        for {
          variar    <- inner(k - 1)   // gives the number of each combination
          elements  <- lista          // elements in each combination
        } yield elements :: variar
    }   // end inner
    inner (n)
  }     // end method

  /**
    * method for presenting any set of list
    * @param conjunto  set of list to be presented
    * @tparam T
    */
  def presentar [T](conjunto : Set[List[T]]) : Unit = {
    val listas = conjunto.toList
    for
    (lista <- listas)
    {println (lista)}
  }
  //--------------------------------------------------------------------------------------------------------------------
}
