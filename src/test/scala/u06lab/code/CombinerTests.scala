package u06lab.code

import org.junit.Test
import org.junit.Assert.*

class CombinerTests:

  private val f: Functions = FunctionsImpl

  @Test
  def testSumFunction() =
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    assertEquals(0.0, f.sum(List()), 0.01) // 0.0

  @Test
  def testConcatFunction() =
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))

  @Test
  def testMaxFunction() =
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.max(List()))

