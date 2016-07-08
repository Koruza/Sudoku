import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  val peersTable = Map(0.to(8).flatMap{
    r=> 0.to(8).map{c=> ((r,c)->calcPeers(r,c))}}:_*)

  def parse(str: String): Board = {
    val empty = Map(0.to(8).flatMap{r=> 0.to(8).map{c=> ((r,c)->1.to(9))}}:_*)
    val stringList = str.toList
    val finalMap = empty.map{
        case((r,c),_) => if (stringList(r*9+c).equals('.'))
                            (r,c)->1.to(9).diff(parseH(peers(r,c),stringList)).toList.sorted
                         else 
                            (r,c)-> List(stringList(r*9+c).asDigit)
    }
    new Board(finalMap)
  }

  def parseH(peer:List[(Int,Int)], str:List[Char]): List[Int] = peer match{
    case Nil=> Nil
    case (head::tail) => if (str.apply(head._1*9+head._2).equals('.')) parseH(tail,str)
                    else (str.apply(head._1*9+head._2).asDigit) :: parseH(tail,str)
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    peersTable((row,col))
  }

  def calcPeers(row: Int,col: Int): List[(Int, Int)] = {
    val boxRow: Int = (row/3)*3
    val boxCol:Int = (col/3)*3
    val boxPeers = (boxRow.to(boxRow+2).flatMap(r=>boxCol.to(boxCol+2).map(c=>(r,c))))
    val rowPeers = 0.to(8).map(r=>(r,col))
    val colPeers = 0.to(8).map(c=>(row,c))
   (rowPeers ++ boxPeers ++ colPeers).filterNot{
    case(r,c) => (r,c) == (row,col)}.toList.distinct
    
  }

}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(available((row,col)).length == 1)
      Some(available((row,col))(0))
    else 
      None
  }

  def isSolved(): Boolean = {
    if(available.forall(x=>x._2.length==1))
      true
    else 
      false
  }

  def isUnsolvable(): Boolean = {
    if(available.exists(x=>x._2.length ==0))
      true
    else 
      false
  }

  // def deParse(b: Board): String = {
  //     val keys = b.available.keys.toList.sorted
  //     deParseHelper(keys, "", b)
  //   }

  //   def deParseHelper(cells: List[(Int,Int)], s: String, b: Board): String = cells match {
  //     case Nil => s+""
  //     case pos :: rest => {
  //       if(b.available(pos).length > 1){
  //         deParseHelper(rest, s+'.', b)
  //       }
  //       else{
  //         val character = b.available(pos)(0).toString
  //         deParseHelper(rest, s+character, b)
  //       } 
  //     }
  //   }

  // def p(row:Int, col:Int, value:Int): Board ={
  //   Solution.parse(deParse(placeH(row,col,value)))
  // }

  // def place(row: Int, col: Int, value: Int): Board = {
  //   p(row,col,value)
  // }

  // def placeH(row: Int, col: Int, value: Int): Board = {
  //   require(availableValuesAt(row, col).contains(value))
  // //  val a = available((row,col)).filter((x:Int) => x == value)
  //   val y = Solution.peers(row,col).foldLeft(available)((acc,v)=>
  //    acc.updated((v),available(v).filter((x:Int)=>x!=value))) 
  //   // val b = y.map(x=>{if(x._2.length == 1) 
  //   //                     place(x._1._1, x._1._2, x._2(0))
  //   //                   else ((x._1._1 , x._1._2)->x._2) })
  //   val z = y.map(x=>{if(x._1._1 == row && x._1._2 ==col) 
  //                       ((row,col)->List(value)) 
  //                     else {
  //                           ((x._1._1 , x._1._2)->x._2)
  //                       }
  //                     }
  //                     )
  //   new Board(z)
  //   //new Board(Rip(z))
  // //  new Board(placeHelper(Solution.peers(row,col)))
  //   //new Board(placeHelper(row,col,value))

  // }

  def placeH(peers: List[(Int, Int)], b: Map[(Int, Int), List[Int]], value: Int): Map[(Int, Int), List[Int]] = peers match {
   case Nil => b
   case head:: tail => {
     val first = b(head)
     val onlyvalue = first.filter((x: Int) => x != value)
     val l = onlyvalue.length
     if (first.length != 1 && l == 1) {
        val x = Solution.peers(head._1, head._2)
        val y = placeH(x, b + ((head._1,head._2) -> onlyvalue), onlyvalue(0))
        placeH(tail, y, value)
     } 
     else
      placeH(tail, b + ((head._1,head._2)-> onlyvalue), value)
     }
   }


  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
        new Board(placeH(Solution.peers(row, col), available + ((row, col) -> List(value)), value))
  }

  // def place1(row:Int, col:Int, value: Int): Map[(Int, Int), List[Int]] = {
  //   val y = Solution.peers(row,col).foldLeft(available)((acc,v)=>
  //    acc.updated((v),available(v).filter((x:Int)=>x!=value))) 
  //   val z = y.map(x=>{if(x._1._1 == row && x._1._2 ==col) 
  //                       ((row,col)->List(value)) 
  //                     else 
  //                       ((x._1._1 , x._1._2)->x._2)
  //                     })
  //   z
  // }

  // def placeHelper(peers: List[(Int,Int)]): Map[(Int, Int), List[Int]] = {
  //   peers match{
  //     case Nil => Map((9,9)->List(1))
  //     case x::tail => if(valueAt(x._1,x._2) == None)
  //                       placeHelper(tail)
  //                     else
  //                       place1(x._1,x._2, available((x._1,x._2))(0)) 
  //  //   case valueAt(row,col) == None => (row,col, available((row,col))) :: placeHelper(row,col,value)
  //   }
  // }

  //You can return any Iterable (e.g., Stream)
  def nextStates(): Stream[Board] = {
    if (isUnsolvable()) {
      Stream()
    }
    else{
      val coordinates = 0.to(8).flatMap{r=> 0.to(8).map{c=> ((r,c))}}.toList
      nextHelper(coordinates.toStream).filterNot(x=>x == isUnsolvable)
    }
  }

  def nextHelper(coord: Stream[(Int,Int)]):Stream[Board]={
    coord match{
      case Stream()=>Stream()
      case (x,y)#::tail => {if(valueAt(x,y) ==None){
                            newB(x,y,available((x,y)).toStream) #::: nextHelper(tail)
                          }
                          else
                            nextHelper(tail)
      }
    }
  }

  def newB(row:Int, col:Int,nums:Stream[Int]): Stream[Board] = {
    nums match{
      case Stream()=>Stream()
      case head #::tail => place(row,col,head) #:: newB(row,col,tail)
    }
  }


  def solve(): Option[Board] = {
    if(isUnsolvable)
      None
    else
     if(isSolved)
        Some(this)
     else{
       val states = nextStates
       solveHelper(states)
     }
  }

  def solveHelper(states: Stream[Board]):Option[Board]={
    states match{
      case Stream() => None
      case head #:: tail => if(isSolved) 
                              Some(head)
                           else{
                            if(head.solve != None)
                              head.solve
                            else
                              solveHelper(tail)
                          }
    }

  }
}