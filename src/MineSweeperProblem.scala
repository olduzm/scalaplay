
object MineSweeperProblem {

  case class MineField(field:Array[Array[Cell]]) {
    override def toString = "Solved mine field would look like:\n" + field.map(_.map(cellToString(_)).mkString).mkString("\n")
  }
  case class Position(x:Int, y:Int)
  sealed abstract case class Cell()
  case object Mine extends Cell
//  case class SafeCell() extends Cell
  case object Unsolved extends Cell
  case class Solved(n:Int) extends Cell

  sealed abstract case class Direction(deltaX:Int, deltaY:Int);
  case object N extends Direction(-1, 0)
  case object NE extends Direction(-1, 1)
  case object E extends Direction(0, 1)
  case object SE extends Direction(1, 1)
  case object S extends Direction(1, 0)
  case object SW extends Direction(1, -1)
  case object W extends Direction(0, -1)
  case object NW extends Direction(-1, -1)

  def cellToString(sf:Cell) : Char = sf match {
    case Mine => '*'
    case Unsolved => '.'
    case Solved(n) => (n + '0').toChar
  } 

  def toField(ch:Char) : Cell = ch match {
    case '*' => Mine
    case '.' => Unsolved
    case _ if(ch.isDigit) => Solved(ch.toInt)
    case _ => sys.error("unrecognised value for mine field: '" + ch + "'")
  }
  def toFields(line:String) : Array[Cell] = line.toCharArray.map(toField(_))
  
  def readToMineField(input:String) : MineField =
    MineField(input.split('\n').map(toFields(_)))
  
  def isValidCoordinate(mf:MineField, x:Int, y:Int): Boolean =
    (x >= 0 && y >= 0 && x < mf.field.length && y < mf.field.head.length)

  def getCellAt(mf:MineField, pos:Position) : Option[Cell] = pos match {
    case Position(x, y) if isValidCoordinate(mf, x, y) => Some(mf.field(x)(y))
    case _ => None
  }
  
  def coordinateOf(pos: Position, d:Direction) = Position(pos.x + d.deltaX, pos.y + d.deltaY)

  def allDirections: List[Direction] = List(N, NE, E, SE, S, SW, W, NW)
  def relativePositions(pos:Position, directions:List[Direction]) = directions.map(coordinateOf(pos, _))

  def getNeighbours(mf:MineField, pos:Position) : List[Cell] =
    relativePositions(pos, allDirections).map(getCellAt(mf, _)).flatMap(x => x)

  def isMine(c:Cell) = c match {
    case Mine => true
    case _ => false
  }

  def countMines(mf:MineField, pos:Position) : Int =
    getNeighbours(mf, pos).count(isMine(_))

  def processCell(mf:MineField, cell:Cell, row:Int, col:Int) : Cell = cell match {
    case Mine => Mine
    case Unsolved => Solved(countMines(mf, Position(row, col)))
    case s:Solved => s
  }
  def processRow(mf:MineField, row:Array[Cell], r:Int) : Array[Cell] =
    row.zipWithIndex.map(t => processCell(mf, t._1, r, t._2))

  def process(mf:MineField) : MineField =
    MineField(mf.field.zipWithIndex.map (t => processRow(mf, t._1, t._2)))

  def main(args: Array[String]) {

    println(
      process(
        readToMineField(
          "*..." + "\n" +
          "...." + "\n" +
          ".*.." + "\n" +
          "...."
        )))

    println(
      process(
        readToMineField(
          "**..." + "\n" +
          "....." + "\n" +
          ".*..."
        )))


  }
}