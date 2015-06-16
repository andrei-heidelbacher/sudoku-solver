package sudoku

import org.scalatest.FunSuite

/**
 * @author andrei
 */
class SolverTests extends FunSuite {
  test("Sample board") {
    val board = Seq(
      "435269781",
      "682501493",
      "197034562",
      "820190307",
      "370682915",
      "951743628",
      "519020874",
      "240907036",
      "763418259")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.toString)
  }

  test("Easy board") {
    val board = Seq(
      "100489006",
      "730000040",
      "000001295",
      "007120600",
      "500703008",
      "006095700",
      "914600000",
      "020000037",
      "800512004")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.toString)
  }

  test("Medium board") {
    val board = Seq(
      "020608000",
      "580009700",
      "000040000",
      "370000500",
      "600000004",
      "008000013",
      "000020000",
      "009800036",
      "000306090")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.toString)
  }

  test("Hard board") {
    val board = Seq(
      "000600400",
      "700003600",
      "000091080",
      "000000000",
      "050180003",
      "000306045",
      "040200060",
      "903000000",
      "020000100")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.toString)
  }

  test("Very hard board") {
    val board = Seq(
      "020000000",
      "000600003",
      "074080000",
      "000003002",
      "080040010",
      "600500000",
      "000010780",
      "500009000",
      "000000040")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.toString)
  }

  test("Empty board") {
    val board = Seq(
      "000000000",
      "000000000",
      "000000000",
      "000000000",
      "000000000",
      "000000000",
      "000000000",
      "000000000",
      "000000000")
    val gameBoard = RegularBoard(board)
    val solution = Solver.solve(gameBoard)
    assert(solution.nonEmpty, "No solution found!")
    println(solution.get.toString)
  }
}
