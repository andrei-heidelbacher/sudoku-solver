package sudoku

import scala.collection.immutable.IndexedSeq

/**
 * Implementation of the classic 9x9 sudoku board.
 *
 * @param board linearized board as sequence of length 81
 * @author andrei
 */
final class RegularBoard(board: IndexedSeq[Int]) extends GameBoard {
  def size: Int = 9

  def constraints: Seq[Seq[(Int, Int)]] = {
    def makeBox(row: Int, column: Int): Seq[(Int, Int)] = {
      for {
        r <- 0 until size / 3
        c <- 0 until size / 3
      } yield (r + row, c + column)
    }

    (0 until size).map(row => (0 until size).map(column => (row, column))) ++
      (0 until size).map(column => (0 until size).map(row => (row, column))) ++
      (for {
        r <- 0 until size by size / 3
        c <- 0 until size by size / 3
      } yield makeBox(r, c))
  }

  def get(row: Int, column: Int): Int = board(row * size + column)

  def updated(row: Int, column: Int, value: Int): RegularBoard =
    RegularBoard(board.updated(row * size + column, value))
}

/**
 * Factory object for class [[sudoku.RegularBoard]]
 */
object RegularBoard {
  /**
   * @param board content of the board saved as a single sequence of 81 values
   * @return board constructed by splitting the given sequence in rows
   */
  def apply(board: IndexedSeq[Int]): RegularBoard = new RegularBoard(board)

  /**
   * @param board sequence of 9 strings of length 9 containing only digits
   * @return board constructed by parsing the sequence of strings
   */
  def apply(board: Seq[String]): RegularBoard = RegularBoard((for {
    row <- board
    value <- row
  } yield value - '0').toIndexedSeq)
}
