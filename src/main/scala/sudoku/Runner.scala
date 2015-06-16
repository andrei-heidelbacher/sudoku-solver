package sudoku

import scala.io.Source

/**
 * Runs the solver for all the puzzles in puzzles.txt and saves the solutions
 * in solutions.txt.
 *
 * @author andrei
 */
object Runner {
  /**
   * @return size of the regular sudoku puzzle
   */
  val size: Int = 9

  /**
   * Parses the boards from input, given a specific format. Every puzzle must
   * begin with a line (usually to state puzzle number) followed by 9 other
   * lines consisting of the puzzle itself.
   *
   * @param fileName text file that contains the puzzles
   * @return parsed boards from input
   */
  def parseInput(fileName: String): Seq[RegularBoard] = {
    val lines = Source.fromFile(fileName).getLines()
    val boards = lines.sliding(size + 1, size + 1).toSeq
    boards.map(b => RegularBoard(b.tail))
  }

  def printSolutions(
      fileName: String,
      solutions: Seq[Option[GameBoard]]): Unit = {
    import java.io._

    val writer = new PrintWriter(new File(fileName))
    solutions.foreach({
      case None => writer.println("No solution found!\n)")
      case Some(board) => writer.println(board.toString + "\n")
    })
    writer.close()
  }

  def main(args: Array[String]): Unit = {
    val inputFileName = "src/main/resources/puzzles.txt"
    val boards = parseInput(inputFileName)
    val solutions = boards.map(Solver.solve)

    val outputFileName = "src/main/resources/solutions.txt"
    printSolutions(outputFileName, solutions)
  }
}
