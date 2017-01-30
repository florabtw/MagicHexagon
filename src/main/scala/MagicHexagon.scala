object MagicHexagon {

  type Piece = Int

  case class Board(rows: List[Row])

  case class Row(pieces: List[Piece])
}
