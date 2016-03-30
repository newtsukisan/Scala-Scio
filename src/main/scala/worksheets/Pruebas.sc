def valid (list: List[Int]) : Boolean = list match {
  case List(op1,op2) if op1 > op2  => true
  case _                           => false
}

valid(List(2,3))
valid(List(3,2))
valid(List(1,2,3))
