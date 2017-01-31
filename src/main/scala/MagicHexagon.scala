object MagicHexagon {

  val magicNumber = 38

  sealed trait Slot

  case class Piece(value: Int) extends Slot {
    override def toString = value.toString
  }
  case class Empty() extends Slot {
    override def toString = " "
  }

  case class State(board: Board, pieces: List[Piece])

  case class Board(rows: List[Row]) {
    def placePiece(slot: Slot, piece: Piece): Board = {
      val row = rows.find(_.slots.exists(_ eq slot)).get
      copy(rows = rows.map {
        case r if r eq row => r.placeIn(slot, piece)
        case r             => r
      })
    }

    def isConsistent = {
      def areMagic(rows: List[Row]) = {
        val fullRows = rows.filter(row => row.isFull)
        fullRows.forall(_.sum == magicNumber)
      }

      areMagic(rows) && areMagic(rotated.rows) && areMagic(rotated.rotated.rows)
    }

    def isFull = rows.forall(_.isFull)

    def mostFullRow: Row = {
      def fullestRow(rows: List[Row]) = rows.minBy(_.emptySlots.length)
      val mostFullRows = List(fullestRow(emptyRows), fullestRow(rotated.emptyRows), fullestRow(rotated.rotated.emptyRows))
      fullestRow(mostFullRows)
    }

    def emptyRows: List[Row] = rows.filter(_.hasSlots)

    private def rotated: Board = Board(rightDiagonals)

    private def rightDiagonals: List[Row] = {
      def nextRow(rows: List[Row]): (Row, List[Row]) = {
        val maxLength  = rows.maxBy(_.slots.length).slots.length
        val rest       = rows.dropWhile(_.slots.length < maxLength).dropWhile(_.slots.length == maxLength)
        val targetRows = rows.take(rows.length - rest.length)
        val nextRow    = Row(targetRows.map(_.slots.last))
        val nextRows   = targetRows.map(_.init).filter(_.slots.nonEmpty) ++ rest
        (nextRow, nextRows)
      }

      def go(rows: List[Row]): List[Row] = nextRow(rows) match {
        case (row, Nil)     => List(row)
        case (row, oldRows) => row :: go(oldRows)
      }

      go(rows)
    }

    override def toString: String = {
      val row1 = rows.head.slots.mkString("  ", " ", "")
      val row2 = rows(1).slots.mkString(" ", " ", "")
      val row3 = rows(2).slots.mkString("", " ", "")
      val row4 = rows(3).slots.mkString(" ", " ", "")
      val row5 = rows(4).slots.mkString("  ", " ", "")
      List(row1, row2, row3, row4, row5).mkString("\n")
    }
  }

  object Board {
    def apply(rows: Row*) = new Board(rows.toList)
  }

  case class Row(slots: List[Slot]) {
    def placeIn(slot: Slot, piece: Piece): Row = copy(slots = slots.map {
      case s if s eq slot => piece
      case s              => s
    })

    val emptySlots: List[Slot] = slots.collect { case e: Empty => e }

    val hasSlots: Boolean = emptySlots.nonEmpty

    val isFull: Boolean = !hasSlots

    def sum: Int = {
      val pieces = slots.filter(_.isInstanceOf[Piece])
      pieces.map(_.asInstanceOf[Piece]).map(_.value).sum
    }

    def init: Row = copy(slots = slots.init)
  }

  object Row {
    def apply(slots: Slot*) = new Row(slots.toList)
  }
}
