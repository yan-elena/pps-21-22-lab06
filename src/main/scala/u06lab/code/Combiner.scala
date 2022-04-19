package u06lab.code

import java.util

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

import Combiners.given

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = combiner(a)
  override def concat(a: Seq[String]): String = combiner(a)
  override def max(a: List[Int]): Int = combiner(a)

  def combiner[A](a: Iterable[A])(using c: Combiner[A]): A = a.foldLeft(c.unit)(c.combine)

/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object Combiners:

  class CombinerImpl[A](value: A, function: (A, A) => A) extends Combiner[A]:
    override def unit: A = value
    override def combine(a: A, b: A): A = function(a, b)

  given Combiner[Double] = CombinerImpl(0.0, _ + _)

  given Combiner[String] = CombinerImpl("", _ + _)

  given Combiner[Int] = CombinerImpl(Int.MinValue, Math.max)

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648
