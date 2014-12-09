def slice [A](m : Int, n : Int, xs:List[A]) : List[A] = {
	def slice_helper [A](m : Int, n : Int, varm : Int , varn : Int, xs : List[A]) : List[A] = {
		(varm,varn) match{
			case (`m`,`n`) => List(xs.head)
			case (`m`,_) => { 
				xs.head::slice_helper(m,n,varm,varn + 1,xs.tail) 
			}
			case (_,_) => slice_helper(m,n,varm+1,varn + 1,xs.tail)
		}
	}
	slice_helper(m,n - 1,0,0,xs)
}

def createList(n: Int): List[Int] = {
	n match {
		case 0 => List()
		case a => createList(a - 1) ::: List(a) 
	}
}

print("This is the slice output : ")
println(slice(0,4,List(2,3,4,5,6)))


def mul35 (n1: Int, n2: Int, lim: Int) : Int = {
	def mul35Helper (rem3: Int, rem5: Int, xs:List[Int], sum:Int):Int = {
		xs match{
			case List() => sum
			case _ => 
			(rem3,rem5) match {
				case (a1,0) => mul35Helper(a1 - 1,4,xs.tail,sum + xs.head)
				case (0,a2) => mul35Helper(2,a2 - 1,xs.tail,sum + xs.head)
				case (a,b) => mul35Helper(a - 1, b - 1, xs.tail, sum)
			}
		}	 
	}
	mul35Helper (n1 - 1, n2 - 1, createList(lim - 1),0)
}

println("This is mul35 output " + mul35(3,5,1000))
