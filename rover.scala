class Rover (var x : Int,var y : Int, var dir: Char) {

  var dirInt: Int = 0
  dir match {
    case 'N' => {
      dirInt = 0
    }
    case 'E' => {
      dirInt = 1
    }
    case 'W' => {
      dirInt = 3
    }
    case 'S' => {
      dirInt = 2
    }

  }

  def move(xs:List[Char]): Boolean = {
    def moveOne(displace: Char): Boolean = {
      (x,dirInt,displace) match {
        case  (0, 3,'M') =>  return false
        case  (5,1,'M') => return false
        case  (_,_,_) => 
      }
      (y, dirInt,displace) match {
        case (0,0,'M') => return false
        case (5,2,'M') => return false
        case (_,_,_) =>  
      }
      displace match {
        case 'L' => {
          dirInt = (dirInt - 1 )
          if (dirInt < 0) dirInt = dirInt + 4
        }
        case 'R' => {
          dirInt = ( dirInt + 1 ) % 4
        }
        case 'M' => {
          dirInt match {
            case 0 => {y = y + 1}
            case 1 => {x = x + 1}
            case 2 => {y = y - 1}
            case 3 => {x = x - 1}
          }
        }
      }
      true
      //if((x < 0) || (x > 5) || (y < 0) || (y > 5)) false else true
    }
    xs match {
      case List() => true
      case z::zs => {
        if (moveOne(z)) move(zs) else false
      }
    }
  }

  override def toString: String = {
    dirInt match {
      case 0 => {
        dir = 'N'
      }
      case 1 => {
        dir = 'E'
      }
      case 2 => {
        dir = 'S'
      }
      case 3 => {
        dir = 'W'
      }
    }
    x + " " + y + " " + dir
  }
}

println("Rover 1")
val r1 = new Rover(1,2,'N')
println(r1.toString)
if (r1.move(List('L','M','L','M','L','M','L','M','M')))
println(r1.toString)
else println("Not possible")

println("Rover 2")
val r2 = new Rover(3,3,'E')
println(r2.toString)
if(r2.move(List('M','M','R','M','M','R','M','R','R','M'))) println(r2.toString) else println ("Not possible")


println("Rover 3")
val r3 = new Rover(0,1,'W')
if (r3.move(List('M')))
println(r3.toString)
else println("Not possible")





