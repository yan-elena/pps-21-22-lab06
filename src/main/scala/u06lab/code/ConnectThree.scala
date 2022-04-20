package u06lab.code

import java.util.OptionalInt

object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(b => b.x == x && b.y == y).map(_.player).orElse(None)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val row = board.filter(_.x == x).map(_.y + 1).maxOption(Ordering[Int]).orElse(Some(0))
    if row.get > bound then None else row

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for x <- 0 to bound
        y = firstAvailableRow(board, x)
        if y.isDefined
    yield board :+ Disk(x, y.get, player)

  //Ex 3
  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq.empty))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        board <- placeAnyDisk(game.head, player)
      yield
        board +: game

  //Ex 4
  def computeAnyGameAndCheckWin(player: Player, moves: Int): LazyList[Game] = (moves match
    case 0 => LazyList(Seq(Seq.empty))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        board <- placeAnyDisk(game.head, player)
      yield
        if isWin(game.head) then game else board +: game).distinct

  def isWin(board: Board): Boolean =
    // row                       column                       diagonal                                anti diagonal
    checkWin(board, _.y, _.x) || checkWin(board, _.x, _.y) || checkWin(board, d => d.x + d.y, _.y) || checkWin(board, d => d.x - d.y, _.y)

  def checkWin(board: Board, groupBy: Disk => Int, map: Disk => Int) =
    val findConsecutive: Seq[Int] => Boolean = s => s.containsSlice(0 until bound) || s.containsSlice(1 to bound)
    board.groupBy(_.player).map(_._2.groupBy(groupBy)).map(_.map(_._2.map(map).sorted(Ordering.Int)).filter(findConsecutive)).exists(_.nonEmpty)

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  println("EX 4: ")

  // Test isWin
  val winSeq = Seq(Seq(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X), Disk(0, 2, X)), // row
    Seq(Disk(0, 3, O), Disk(0, 2, O), Disk(0, 1, O), Disk(0, 0, X), Disk(1, 1, O)), // column
    Seq(Disk(3, 0, X), Disk(2, 1, X), Disk(1, 2, X), Disk(0, 3, O), Disk(0, 2, O)), // anti diagonal
    Seq(Disk(0, 0, O), Disk(1, 1, O), Disk(2, 2, O), Disk(0, 3, X), Disk(0, 2, O))) // anti diagonal

  printBoards(winSeq)

  for board <- winSeq do
    println(isWin(board)) // true

  val notWinSeq = Seq(Seq(Disk(0, 1, X), Disk(1, 0, X), Disk(2, 0, X), Disk(0, 2, X)), // row
    Seq(Disk(0, 3, O), Disk(1, 2, O), Disk(0, 1, O), Disk(0, 0, X), Disk(1, 1, O)), // column
    Seq(Disk(3, 0, X), Disk(2, 2, X), Disk(1, 2, X), Disk(0, 3, O), Disk(0, 2, O)), // anti diagonal
    Seq(Disk(0, 2, O), Disk(1, 1, O), Disk(2, 2, O), Disk(0, 3, X), Disk(0, 2, O))) // anti diagonal

  printBoards(notWinSeq)

  for board <- notWinSeq do
    println(isWin(board)) // false

  // compare initial computeAnyGame and computeAnyGameAndCheckWin
  val computeAnyGame1 = computeAnyGame(O, 7)
  val computeAnyGame2 = computeAnyGameAndCheckWin(O, 7)

  println("Initial size: " + computeAnyGame1.size + " Final size: " + computeAnyGame2.size) // 15540 to 13764
