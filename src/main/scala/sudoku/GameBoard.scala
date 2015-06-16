package sudoku

/**
 * Contains basic operations on a sudoku board and allows creating custom rules
 * by extending the class.
 *
 * @author andrei
 */
abstract class GameBoard {
  require(isValidGame)

  /**
   * @return value for empty cells
   */
  final def empty: Int = 0

  /**
   * @return size of the square board
   */
  def size: Int

  /**
   * @return sequence containing constraint groups, each of length
   * [[sudoku.GameBoard.size]]
   */
  def constraints: Seq[Seq[(Int, Int)]]

  protected final def isValidValue(value: Int): Boolean =
    value == empty || (1 <= value && value <= size)

  protected final def isInside(row: Int, column: Int): Boolean =
    0 <= row && row < size && 0 <= column && column < size

  protected final def isInside(cell: (Int, Int)): Boolean = cell match {
    case (row, column) => isInside(row, column)
  }

  /**
   * Checks that this board contains valid values and all constraint groups have
   * [[sudoku.GameBoard.size]] distinct cells inside the board.
   *
   * @return
   */
  private def isValidGame: Boolean = {
    size > 0 &&
      (for {
        r <- 0 until size
        c <- 0 until size
      } yield get(r, c)).forall(isValidValue) &&
      constraints.forall(group =>
        group.distinct.length == group.length &&
          group.length == size &&
          group.forall(isInside))
  }

  /**
   * Checks that all constraints are fulfilled.
   *
   * @return
   */
  final def isValidBoard: Boolean =
    constraints.forall(group =>
      (1 to size).forall(value => group.count(get(_) == value) <= 1))

  /**
   * @return sequence with all empty cells
   */
  final def emptyCells: Seq[(Int, Int)] = for {
    r <- 0 until size
    c <- 0 until size
    if get(r, c) == empty
  } yield (r, c)

  def get(row: Int, column: Int): Int

  final def get(cell: (Int, Int)): Int = cell match {
    case (r, c) => get(r, c)
  }

  def updated(row: Int, column: Int, value: Int): GameBoard

  final def updated(cell: (Int, Int), value: Int): GameBoard = cell match {
    case (r, c) => updated(r, c, value)
  }

  final def isSolution: Boolean =
    (1 to size).forall(value =>
      constraints.forall(_.count(get(_) == value) == 1))

  override def toString: String =
    (0 until size)
      .map(r => (0 until size).map(get(r, _)).mkString(" "))
      .mkString("\n")
}
