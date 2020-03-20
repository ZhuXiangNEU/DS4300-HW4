// Xiang Zhu
// DS4300 HW4 Part A: 2

import breeze.plot._

/*
Write a recursive function to convert an unsigned integer to an n-bit binary string.
What is the 32-bit binary for 1,234,567,890? The number of 1’s in a binary number is known
as the weight. Produce a plot of the weight of binary numbers from 0 to 1024.

toBinary(x: Int, bits: Int): String
weight(b: String): Int
*/
object Binary extends App {

  // convert an unsigned integer to an n-bit binary string
  def toBinary(x: Int, bits: Int): String = {
    if (x.toBinaryString.length() > bits)
      throw new IllegalArgumentException("Unsigned integer is out of boundary.")
    else
     "0" * (bits - x.toBinaryString.length()) + x.toBinaryString
  }

  println(toBinary(1,8))
  println(toBinary(0, 8))
  println(toBinary(88, 8))
  println(toBinary(257, 10))

  // The number of 1’s in a binary number is known as the weight.
  def weight(b: String): Int = {
    b.count(_ == '1')
  }

  println(weight("101"))
  println(weight("000011"))
  println(weight("000000"))
  println(weight("1"))
  println(weight("0"))

  // Produce a plot of the weight of binary numbers from 0 to 1024.
  val xs = Range(0,1025)
  val ys = xs.map(x => weight(toBinary(x,16)))
  val fig = Figure()
  val plt = fig.subplot(0)
  plt += plot(xs,ys)
  fig.refresh()
}
