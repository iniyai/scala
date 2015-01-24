def fact(num: Int):Int = {
  num match {
    case 1 => num
    case a => a * fact(a - 1)
  }
}

//demo git

def findk (xs:List[Int],k:Int):Int = {
  k match {
    case 0 => xs.head
    case _ => findk(xs.tail,k - 1)
  }
}

def findklist (xs:List[Int],k:Int):List[Int] = {
  k match {
    case 0 => xs
    case _ => findklist(xs.tail,k - 1)
  }
}

def length (xs:List[Int]):Int = {
  xs match {
    case z::zs => 1+ length(zs)
    case List() => 0
  }
} 

def removeklist (xs:List[Int],k:Int):List[Int] = {
  k match {
    case 0 => xs.tail
    case _ => xs.head::removeklist(xs.tail,k - 1)
  }
}

def permut (xs:List[Int],num:Int) : List [Int] = {
  println("hi")
  xs match {
    case x::y::Nil => {
      if(num == 0) return x::y::Nil else return { y::List(x)}
    }
    case _ => {
      val bucket:Int = num / fact(length(xs) - 1)
      val rem:Int = num % fact(length(xs) - 1)
      val newlist:List[Int] = removeklist(xs, bucket)
      println(newlist)
      return bucket::permut(newlist, rem)
    }
  }
}

println("permutt test")
println(permut(List(0,1,2),3))



