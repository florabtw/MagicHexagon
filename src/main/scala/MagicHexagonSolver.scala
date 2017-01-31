import MagicHexagon._

case class MagicHexagonSolver(state: State) {
  def solved: Board = {
    solve(state).get
  }

  type Placement = (Board, Piece) => Board

  private def choosePlacement(board: Board): Placement = {
    val row  = board.mostFullRow
    val slot = row.emptySlots.head
    placePiece(slot)
  }

  private def placePiece(slot: Slot)(board: Board, piece: Piece): Board = {
    board.placePiece(slot, piece)
  }

  private def solve(state: State): Option[Board] = state match {
    case State(board, Nil)                        => Some(board)
    case State(board, _  ) if !board.isConsistent => None
    case State(board, pieces)                     =>
      val placement = choosePlacement(board)
      val children  = for (piece <- pieces) yield State(placement(board, piece), pieces.filter(_ != piece))
      children.foldLeft[Option[Board]](None) { (a, b) => a.orElse(solve(b)) }
  }
}
