import scala.Predef._

object FishGame {

  case class Position(x: Int, y:Int) {
    override def toString = "[" + x + "," + y + "]"
  }

  abstract sealed case class Player()
  case object FirstPlayer extends Player {
    override def toString = " A"
  }

  case object SecondPlayer extends Player {
    override def toString = " B"
  }

  sealed abstract case class PositionState()
  case class Ice(player:Option[Player]) extends PositionState {
    override def toString = player match {
      case Some(p) => p toString
      case None => " #"
    }
  }
  case object NoIce extends PositionState {
    override def toString = " ."
  }

  abstract sealed case class Direction()
  case object N extends Direction
  case object S extends Direction
  case object E extends Direction
  case object W extends Direction

  case class Board(w:Int, h:Int, positionStates:Map[Position, PositionState]) {
    override def toString = positionStates.toList.sortBy(t=>(t._1.x + t._1.y * h)).map(_._2)
      .grouped(w).map(_.mkString)
      .mkString("\n")

    def positionsForPlayer(player:Player) : List[Position] =
      positionStates.toList.filter((pair) => Board.playerAt(this, pair._1) == Some(player)).map(_._1)
  }

  object Board {
    def create(w:Int, h:Int, players:List[(Player, List[Position])]) : Board = {
      val allPositions = for (x <- 1 until w+1; y <- 1 until h+1) yield Position(x, y)
      val initialMap:Map[Position, PositionState] = allPositions.map((_, Ice(None))).toMap
      Board(w, h, players.foldLeft(initialMap)(addPenguinsToMap(_, _)))
    }

    def playerAt(b:Board, orig:Position) : Option[Player] = {
      posStateFor(b, orig) match {
        case Ice(Some(player)) => Some(player)
        case _ => None
      }
    }

    def makeMove(b:Board, orig:Position, dest:Position) : Board = {
      if (isFree(b, dest))
        Board(b.w, b.h, posStateAfterMove(b, orig, dest, playerAt(b, orig)))
      else
        error("Cannot move to " + dest + " as it is not available " + posStateFor(b, dest))
    }


    def isFree(b:Board, pos:Position) : Boolean =
      posStateFor(b, pos) match {
        case Ice(None) => true
        case _ => false
      }

    private def addPenguinsToMap(m:Map[Position, PositionState], item:(Player, List[Position])) : Map[Position, PositionState] = {
      val positions : List[Position] = item._2
      positions.foldLeft(m)(addSinglePenguinToMap(_, item._1, _))
    }

    private def addSinglePenguinToMap(m:Map[Position, PositionState], player:Player, pos:Position) : Map[Position, PositionState] = {
      m + ((pos, Ice(Some(player))))
    }

    private def posStateFor(b: FishGame.Board, orig: FishGame.Position): FishGame.PositionState = {
      b.positionStates.get(orig) match {
        case Some(state) => state
        case None => error("this should not happen")
      }
    }

    private def posStateAfterMove(board:Board, orig:Position, dest:Position, playerO:Option[Player]) : Map[Position, PositionState] = {
      playerO match {
        case Some(player) => board.positionStates + ((orig, NoIce)) + ((dest, Ice(Some(player))))
        case None => error("Cannot start a move from a location with no player")
      }
    }
  }

  def isPositionValid(b:Board, p:Position) : Boolean =
    p.x > 0 && p.x <= b.w &&
    p.y > 0 && p.y <= b.h

  def tryPosition(b:Board, x:Int, y:Int):Option[Position] =
    Position(x, y) match {
      case p if isPositionValid(b, p) => Some(p)
      case _ => None
    }

  def nextPositionOnDirection(b:Board, pos:Position, d:Direction) : Option[Position] =
    d match {
      case N => tryPosition(b, pos.x, pos.y - 1)
      case S => tryPosition(b, pos.x, pos.y + 1)
      case E => tryPosition(b, pos.x + 1, pos.y)
      case W => tryPosition(b, pos.x - 1, pos.y)
    }

  def onDirection(b:Board, pos:Position, d:Direction) : List[Option[Position]] =
    nextPositionOnDirection(b, pos, d) match {
      case Some(nextP) if isPositionValid(b, nextP) => Some(nextP)::onDirection (b, nextP, d)
      case _ => Nil
    }

  def allPossibleMoves(b:Board, pos:Position) : List[Position] =
    List(N, S, E, W).map(onDirection(b, pos, _)).flatten.flatten

  def initBoard() = {
    Board.create(4, 4, List(
      (FirstPlayer, List(Position(1, 1), Position(4, 3))),
      (SecondPlayer, List(Position(2, 2), Position(3, 4))))
    )
  }

  def say(msg:String, board:Board) {
    Console println msg + "\n" + board
  }

  def print(msg:Any) {
    Console println msg
  }

  def main(args: Array[String]) {
    val b = initBoard()
    say("initial board:", b)
    print("penguins for p1:" + b.positionsForPlayer(FirstPlayer))
    print("penguins for p2:" + b.positionsForPlayer(SecondPlayer))

    print(allPossibleMoves(b, Position(1,1)))
    say("after first move:", Board.makeMove(b, Position(1,1), Position(4,1)))

  }
}