package sudoku

/**
 * Simple solver that uses only one heuristic. For every empty cell, it finds
 * all the possible values and starts completing the cell with the least number
 * of possibilities.
 *
 * @author andrei
 */
object Solver {
  protected final def possibleValues(
      board: GameBoard,
      cell: (Int, Int)): Seq[Int] = {
    val containingGroups = board.constraints.filter(_.contains(cell))
    (1 to board.size).filter(v =>
      containingGroups.forall(!_.map(board.get).contains(v)))
  }

  protected final def possibleValues(
      board: GameBoard,
      row: Int,
      column: Int): Seq[Int] =
    possibleValues(board, (row, column))

  /**
   * @param board puzzle to solve
   * @return optionally returns the first found solution
   */
  def solve(board: GameBoard): Option[GameBoard] = {
    if (board.isSolution) {
      Some(board)
    } else if (!board.isValidBoard) {
      None
    } else {
      val ((row, column), values) = board.emptyCells
        .map(cell => (cell, possibleValues(board, cell)))
        .sortBy({ case (cell, v) => v.length })
        .head
      val newBoards = (for {
        v <- values
        newBoard = board.updated(row, column, v)
        if newBoard.isValidBoard
      } yield newBoard).toStream
      newBoards.map(solve).find(_.isDefined).flatten
    }
  }
}
