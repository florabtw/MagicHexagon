import MagicHexagon._

object Main extends App {

  val board = Board(
    Row(Empty(), Empty(), Empty()),
    Row(Empty(), Empty(), Empty(), Empty()),
    Row(Empty(), Empty(), Empty(), Empty(), Empty()),
    Row(Empty(), Empty(), Empty(), Empty()),
    Row(Empty(), Empty(), Empty())
  )

  val pieces = for (i <- 1 to 19) yield Piece(i)

  val state = State(board, pieces.toList)

  val solver = MagicHexagonSolver(state)

  println(solver.solved)
}
