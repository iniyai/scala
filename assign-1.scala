val numList = List(1,2,6,5,7,5,46,3,53,8,12)

def last (xs: List[Any]): Any = xs match{
  case List() => List()
  case y::List() => y
  case y::ys => last(ys)
}

def seclast [A](xs: List[A]): A = xs match{
  case List(a,b) => a
  case y::ys => seclast(ys)
}

def reverse [A](xs:List[A], ys:List[A] = List()) : List[A] = xs match {
    case List() => ys
    case z::zs => reverse(zs,z::ys)
}

def printList [A](xs: List[A]):Unit = xs match {
  case y :: ys =>
  {println(y)
    printList (ys)}
  case List() => {}
}

def dedup(xs: List[Any]) : List[Any] = xs match {
  case List() => List()
  case a::b::ys if(a == b)=> dedup(b::ys)
  case a::ys => a::dedup(ys)
}

def dup(num : Int, xs : List[Any]) : List[Any] =
{
  def dup1(n1:Int,zs:List[Any]) : List[Any] = n1 match{
    case 1 => zs
    case _ => zs.head::dup1(n1 - 1, zs)
  }
  xs match
{

    case List(a) => dup1(num,xs)
    case a::ys => dup1(num,List(a))++ dup(num,ys)
  }

}

def dropn1 (xs:List[Any], num:Int,varnum:Int):List[Any] = xs match{
  case List() => List()
  case y::ys => varnum match{
    case 0 => dropn1(ys,num,num-1)
    case _ => y::dropn1(ys,num,varnum - 1)
  }
}

def dropn(x:List[Any], num:Int):List[Any] = {
  dropn1(x,num,num - 1)
}

//println("dsfd");
//println(dropn(List(1,2,3,4),2))
/*def slice(n1:Int,n2:Int,v1:Int,v2:Int,xs:List[Any]):List[Any] = (v1,v2) match{
    case(n1,n2) => xs
    case(n1,_) =>

    case(_,_) => xs match{
        case z:zs => slice(zs)
    }
}*/



def isprime(num:Int):Boolean = {
  def isprime_iter(iter:Int,num:Int):Boolean = iter match{
    case 1 => true
    case _ => if((num % iter) != 0) isprime_iter(iter - 1, num) else false
  }
  isprime_iter(num - 1, num)
}

def primeList(n1:Int, n2:Int): List[Int] = n1 match{
  case `n2` => if(isprime(n2)) List(n2) else List()
  case _ => if (isprime(n1)) n1::primeList(n1 + 1, n2) else primeList(n1 + 1, n2)
}

def Engwords(n: Int) : List[String] =  
{
  def engTrans(n:Int) : String = n match {
    case 1 => "One"
    case 2 => "Two"
    case 3 => "Three"
    case 4 => "Four"
    case 5 => "Five"
    case 6 => "Six"
    case 7 => "Seven"
    case 8 => "Eight"
    case 9 => "Nine"
    case 0 => "Zero"
  }
  n match{
    case 0 => List()
    case _ => Engwords(n/10) ++ List(engTrans(n%10))
  }
}

def gridComputer(num: Int): Int = {
  def grid(m: Int, n:Int): Int = (m,n) match {
    case (0, 0) => 0
    case (n, 0) => 1
    case (0, n) => 1
    case (m, n) => grid(m, n - 1) + grid(m - 1, n) 
  } 
  grid(num,num)
}

println(gridComputer(5))

val checknum = 1234

println(Engwords( checknum))


